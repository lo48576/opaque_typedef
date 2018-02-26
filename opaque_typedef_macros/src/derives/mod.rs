//! Derive targets.

use quote;
use quote::ToTokens;
use syn;

use attrs::{get_meta_content_by_path, is_attr_with_path};
use type_props::TypeProps;

mod as_ref;
mod deref;
mod fmt;


/// Auto-derive target trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, AsRefStr, EnumString)]
pub enum Derive {
    /// `std::ascii::AsciiExt for Outer`.
    AsciiExt,
    /// `AsMut<DerefTarget> for Outer`.
    AsMutDeref,
    /// `AsMut<Inner> for Outer`.
    AsMutInner,
    /// `AsMut<Outer> for Outer`.
    AsMutSelf,
    /// `AsRef<DerefTarget> for Outer`.
    AsRefDeref,
    /// `AsRef<Inner> for Outer`.
    AsRefInner,
    /// `AsRef<Self> for Outer`.
    AsRefSelf,
    /// `std::fmt::Binary for Outer`.
    Binary,
    /// `Default for &Outer`.
    DefaultRef,
    /// `std::ops::Deref for Outer`.
    Deref,
    /// `std::ops::DerefMut for Outer`.
    DerefMut,
    /// `std::fmt::Display for Outer`.
    Display,
    /// `From<Inner> for Outer`.
    FromInner,
    /// `From<Outer> for Arc<Outer>`.
    IntoArc,
    /// `From<Outer> for Box<Outer>`.
    IntoBox,
    /// `From<Outer> for Inner`.
    IntoInner,
    /// `From<Outer> for Rc<Outer>`.
    IntoRc,
    /// `std::fmt::LowerExp for Outer`.
    LowerExp,
    /// `std::fmt::LowerHex for Outer`.
    LowerHex,
    /// `std::fmt::Octal for Outer`.
    Octal,
    /// `PartialEq<Inner> for Outer` and similar ones.
    PartialEqInner,
    /// `PartialEq<Cow<Inner>> for Outer` and similar ones.
    PartialEqInnerCow,
    /// `PartialEq<Cow<Outer>> for Outer` and similar ones.
    PartialEqSelfCow,
    /// `PartialEq<Cow<Outer>> for Inner` and similar ones.
    PartialEqSelfCowAndInner,
    /// `PartialOrd<Inner> for Outer` and similar ones.
    PartialOrdInner,
    /// `PartialOrd<Cow<Inner>> for Outer` and similar ones.
    PartialOrdInnerCow,
    /// `PartialOrd<Cow<Outer>> for Outer` and similar ones.
    PartialOrdSelfCow,
    /// `PartialOrd<Cow<Outer>> for Inner` and similar ones.
    PartialOrdSelfCowAndInner,
    /// `std::fmt::Pointer for Outer`.
    Pointer,
    /// `std::fmt::UpperExp for Outer`.
    UpperExp,
    /// `std::fmt::UpperHex for Outer`.
    UpperHex,
}

impl Derive {
    /// Get derive targets from the given attributes.
    pub fn from_attrs(attrs: &[syn::Attribute]) -> Vec<Self> {
        let mut derives = Vec::new();
        let metaitems = get_derive_meta(attrs);
        for metaitem in &metaitems {
            match *metaitem {
                syn::Meta::Word(ref ident) => {
                    let target = ident.as_ref();
                    match target.parse::<Derive>() {
                        Ok(v) => derives.push(v),
                        Err(_) => {
                            abort_on_unknown_derive_target(format_args!("{}", target));
                        },
                    }
                },
                syn::Meta::List(ref metalist) => {
                    let parent = metalist.ident.as_ref();
                    for nested_meta in &metalist.nested {
                        let meta = match *nested_meta {
                            syn::NestedMeta::Meta(ref meta) => meta,
                            syn::NestedMeta::Literal(ref lit) => {
                                abort_on_unsupported_derive_format(format_args!(
                                    "{}({})",
                                    parent,
                                    lit.into_tokens()
                                ));
                            },
                        };
                        match *meta {
                            syn::Meta::Word(ref ident) => Self::append_from_nested_names(
                                parent,
                                &[ident.as_ref()],
                                &mut derives,
                            ),
                            syn::Meta::List(ref metalist) => abort_on_unsupported_derive_format(
                                format_args!("{}({})", parent, metalist.into_tokens()),
                            ),
                            syn::Meta::NameValue(ref namevalue) => {
                                abort_on_unsupported_derive_format(format_args!(
                                    "{}({})",
                                    parent,
                                    namevalue.into_tokens()
                                ))
                            },
                        }
                    }
                },
                syn::Meta::NameValue(ref namevalue) => {
                    abort_on_unsupported_derive_format(format_args!("{}", namevalue.into_tokens()));
                },
            }
        }
        derives
    }

    /// Returns derive targets specified by `parent(child1, child2, ...)` style.
    fn append_from_nested_names(parent: &str, children: &[&str], derives: &mut Vec<Self>) {
        match parent {
            "PartialEq" => {
                derives.extend(children.into_iter().map(|&child| match child {
                    "Inner" => Derive::PartialEqInner,
                    "InnerCow" => Derive::PartialEqInnerCow,
                    "SelfCow" => Derive::PartialEqSelfCow,
                    "SelfCowAndInner" => Derive::PartialEqSelfCowAndInner,
                    _ => abort_on_unknown_derive_target(format_args!("{}({})", parent, child)),
                }));
            },
            "PartialOrd" => {
                derives.extend(children.into_iter().map(|&child| match child {
                    "Inner" => Derive::PartialOrdInner,
                    "InnerCow" => Derive::PartialOrdInnerCow,
                    "SelfCow" => Derive::PartialOrdSelfCow,
                    "SelfCowAndInner" => Derive::PartialOrdSelfCowAndInner,
                    _ => abort_on_unknown_derive_target(format_args!("{}({})", parent, child)),
                }));
            },
            _ => abort_on_unknown_derive_target(format_args!("{}(..)", parent)),
        }
    }

    /// Generates impls for the auto-derive target.
    pub fn impl_auto_derive(&self, props: &TypeProps) -> quote::Tokens {
        match (*self, props.inner_sizedness) {
            // `std::fmt::*` traits.
            (Derive::Binary, _)
            | (Derive::Display, _)
            | (Derive::LowerExp, _)
            | (Derive::LowerHex, _)
            | (Derive::Octal, _)
            | (Derive::Pointer, _)
            | (Derive::UpperExp, _)
            | (Derive::UpperHex, _) => fmt::gen_impl(*self, props),
            // `std::ops::Deref*` traits.
            (Derive::Deref, _) | (Derive::DerefMut, _) => deref::gen_impl(*self, props),
            // `std::conevert::As*` traits.
            (Derive::AsMutDeref, _)
            | (Derive::AsMutInner, _)
            | (Derive::AsMutSelf, _)
            | (Derive::AsRefDeref, _)
            | (Derive::AsRefInner, _)
            | (Derive::AsRefSelf, _) => as_ref::gen_impl(*self, props),
            _ => unimplemented!("auto-derive for `{}`", self.as_ref()),
        }
    }
}


/// Returns metadata in `#[opaque_typedef(derive(..))]` (`..` part).
fn get_derive_meta(attrs: &[syn::Attribute]) -> Vec<syn::Meta> {
    attrs
        .into_iter()
        .filter(|attr| is_attr_with_path(attr, &["opaque_typedef"]))
        .filter_map(|attr| attr.interpret_meta())
        .flat_map(|meta| get_meta_content_by_path(meta, &["opaque_typedef", "derive"]))
        .filter_map(|nested_meta| match nested_meta {
            syn::NestedMeta::Meta(meta) => Some(meta),
            syn::NestedMeta::Literal(..) => None,
        })
        .collect()
}


fn abort_on_unknown_derive_target<'a>(target: ::std::fmt::Arguments<'a>) -> ! {
    panic!(
        "`#[opaque_typedef(derive({target}))]` is specified, but the target `{target}` is unknown",
        target = target
    );
}


fn abort_on_unsupported_derive_format<'a>(inner: ::std::fmt::Arguments<'a>) -> ! {
    panic!(
        "`#[opaque_typedef(derive({}))]` is specified, but this format is not supported",
        inner
    );
}
