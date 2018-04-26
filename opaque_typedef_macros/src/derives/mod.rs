//! Derive targets.

use std::borrow::Cow;
use std::collections::HashMap;

use quote;
use quote::ToTokens;
use syn;

use attrs::{get_meta_content_by_path, is_attr_with_path};
use type_props::{Sizedness, TypeProps};
use utils::extend_generics;

use self::ops::OpSpec;

mod as_ref;
mod cmp;
mod convert;
mod deref;
mod fmt;
mod ops;


/// Auto-derive target trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, AsRefStr, EnumString,
         EnumProperty)]
pub enum Derive {
    /// `std::ops::Add<Outer> for Outer`.
    #[strum(props(op = "Add", lhs = "Outer", rhs = "Outer", variation = "Direct"))]
    AddSelf,
    /// `std::ops::Add<Inner> for Outer`.
    #[strum(props(op = "Add", lhs = "Outer", rhs = "Inner", variation = "Direct"))]
    AddInner,
    /// `std::ops::Add<Outer> for Inner`.
    #[strum(props(op = "Add", lhs = "Inner", rhs = "Outer", variation = "Direct"))]
    AddInnerRev,
    /// `std::ops::Add<Outer> for Outer` variations.
    #[strum(props(op = "Add", lhs = "Outer", rhs = "Outer", variation = "References"))]
    AddRefSelf,
    /// `std::ops::Add<Inner> for Outer` variations.
    #[strum(props(op = "Add", lhs = "Outer", rhs = "Inner", variation = "References"))]
    AddRefInner,
    /// `std::ops::Add<Outer> for Inner` variations.
    #[strum(props(op = "Add", lhs = "Inner", rhs = "Outer", variation = "References"))]
    AddRefInnerRev,
    /// `std::ops::AddAssign<Outer> for Outer`.
    #[strum(props(op = "AddAssign", lhs = "Outer", rhs = "Outer", variation = "Direct"))]
    AddAssignSelf,
    /// `std::ops::AddAssign<Inner> for Outer`.
    #[strum(props(op = "AddAssign", lhs = "Outer", rhs = "Inner", variation = "Direct"))]
    AddAssignInner,
    /// `std::ops::AddAssign<&Outer> for Outer`.
    #[strum(props(op = "AddAssign", lhs = "Outer", rhs = "Outer", variation = "References"))]
    AddAssignRefSelf,
    /// `std::ops::AddAssign<&Inner> for Outer`.
    #[strum(props(op = "AddAssign", lhs = "Outer", rhs = "Inner", variation = "References"))]
    AddAssignRefInner,
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
    /// `PartialEq<Outer> for Inner` and similar ones.
    PartialEqInnerRev,
    /// `PartialEq<Cow<Inner>> for Outer` and similar ones.
    PartialEqInnerCow,
    /// `PartialEq<Outer> for Cow<Inner>` and similar ones.
    PartialEqInnerCowRev,
    /// `PartialEq<Outer> for Outer` and similar ones.
    PartialEqSelf,
    /// `PartialEq<Cow<Outer>> for Outer` and similar ones.
    PartialEqSelfCow,
    /// `PartialEq<Outer> for Cow<Outer>` and similar ones.
    PartialEqSelfCowRev,
    /// `PartialEq<Cow<Outer>> for Inner` and similar ones.
    PartialEqSelfCowAndInner,
    /// `PartialEq<Inner> for Cow<Outer>` and similar ones.
    PartialEqSelfCowAndInnerRev,
    /// `PartialOrd<Inner> for Outer` and similar ones.
    PartialOrdInner,
    /// `PartialOrd<Outer> for Inner` and similar ones.
    PartialOrdInnerRev,
    /// `PartialOrd<Cow<Inner>> for Outer` and similar ones.
    PartialOrdInnerCow,
    /// `PartialOrd<Outer> for Cow<Inner>` and similar ones.
    PartialOrdInnerCowRev,
    /// `PartialOrd<Outer> for Outer` and similar ones.
    PartialOrdSelf,
    /// `PartialOrd<Cow<Outer>> for Outer` and similar ones.
    PartialOrdSelfCow,
    /// `PartialOrd<Outer> for Cow<Outer>` and similar ones.
    PartialOrdSelfCowRev,
    /// `PartialOrd<Cow<Outer>> for Inner` and similar ones.
    PartialOrdSelfCowAndInner,
    /// `PartialOrd<Inner> for Cow<Outer>` and similar ones.
    PartialOrdSelfCowAndInnerRev,
    /// `std::fmt::Pointer for Outer`.
    Pointer,
    /// `std::cmp::Ord for Outer`.
    Ord,
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
        lazy_static! {
            static ref NESTED_DERIVES: HashMap<&'static str, HashMap<&'static str, Derive>> = {
                // Note that `Self` cannot be used for child target, because the
                // token `Self` cannot be an identifier but parsed as
                // identifier.
                const TARGETS: &[(&'static str, &'static [(&'static str, Derive)])] = &[
                    ("AsMut", &[
                        ("Deref", Derive::AsMutDeref),
                        ("Inner", Derive::AsMutInner),
                        ("Self_", Derive::AsMutSelf),
                    ]),
                    ("AsRef", &[
                        ("Deref", Derive::AsRefDeref),
                        ("Inner", Derive::AsRefInner),
                        ("Self_", Derive::AsRefSelf),
                    ]),
                    ("Into", &[
                        ("Arc", Derive::IntoArc),
                        ("Box", Derive::IntoBox),
                        ("Inner", Derive::IntoInner),
                        ("Rc", Derive::IntoRc),
                    ]),
                    ("PartialEq", &[
                        ("Inner", Derive::PartialEqInner),
                        ("InnerRev", Derive::PartialEqInnerRev),
                        ("InnerCow", Derive::PartialEqInnerCow),
                        ("InnerCowRev", Derive::PartialEqInnerCowRev),
                        ("Self_", Derive::PartialEqSelf),
                        ("SelfCow", Derive::PartialEqSelfCow),
                        ("SelfCowRev", Derive::PartialEqSelfCowRev),
                        ("SelfCowAndInner", Derive::PartialEqSelfCowAndInner),
                        ("SelfCowAndInnerRev", Derive::PartialEqSelfCowAndInnerRev),
                    ]),
                    ("PartialOrd", &[
                        ("Inner", Derive::PartialOrdInner),
                        ("InnerRev", Derive::PartialOrdInnerRev),
                        ("InnerCow", Derive::PartialOrdInnerCow),
                        ("InnerCowRev", Derive::PartialOrdInnerCowRev),
                        ("Self_", Derive::PartialOrdSelf),
                        ("SelfCow", Derive::PartialOrdSelfCow),
                        ("SelfCowRev", Derive::PartialOrdSelfCowRev),
                        ("SelfCowAndInner", Derive::PartialOrdSelfCowAndInner),
                        ("SelfCowAndInnerRev", Derive::PartialOrdSelfCowAndInnerRev),
                    ]),
                    ("Add", &[
                        ("Self_", Derive::AddSelf),
                        ("Inner", Derive::AddInner),
                        ("InnerRev", Derive::AddInnerRev),
                    ]),
                    ("AddRef", &[
                        ("Self_", Derive::AddRefSelf),
                        ("Inner", Derive::AddRefInner),
                        ("InnerRev", Derive::AddRefInnerRev),
                    ]),
                    ("AddAssign", &[
                        ("Self_", Derive::AddAssignSelf),
                        ("Inner", Derive::AddAssignInner),
                    ]),
                    ("AddAssignRef", &[
                        ("Self_", Derive::AddAssignRefSelf),
                        ("Inner", Derive::AddAssignRefInner),
                    ]),
                ];
                TARGETS.into_iter().map(|&(parent, subtargets)| {
                    (parent, subtargets.into_iter().map(|&v| v).collect())
                }).collect()
            };
        }

        let submap = NESTED_DERIVES.get(parent).unwrap_or_else(|| {
            abort_on_unknown_derive_target(format_args!("{}(..)", parent));
        });
        derives.extend(children.into_iter().map(|&child| {
            submap.get(child).unwrap_or_else(|| {
                abort_on_unknown_derive_target(format_args!("{}({})", parent, child));
            })
        }));
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
            // `std::convert::{From, Into}` traits.
            (Derive::FromInner, _) => convert::gen_impl_from_inner(props),
            (Derive::IntoArc, _) | (Derive::IntoBox, _) | (Derive::IntoRc, _) => {
                match props.inner_sizedness {
                    Sizedness::Sized => panic!(
                        "`#[opaque_typedef(derive({}))]` is not supported for sized types",
                        self.as_ref()
                    ),
                    Sizedness::Unsized => convert::gen_impl_into_smartptr(*self, props),
                }
            },
            (Derive::IntoInner, _) => convert::gen_impl_into_inner(props),
            // `std::default::Default` trait.
            (Derive::DefaultRef, Sizedness::Sized) => panic!(
                "`#[opaque_typedef(derive({}))]` is not supported for sized types",
                self.as_ref()
            ),
            (Derive::DefaultRef, Sizedness::Unsized) => {
                let ty_outer = props.ty_outer.into_tokens();
                let type_generics = &props.type_generics;
                let (generics, new_lifetimes) =
                    extend_generics(Cow::Borrowed(props.generics), 1, &[]);
                let new_lt = new_lifetimes[0];
                let ty_inner = props.field_inner.ty().into_tokens();
                let extra_preds = if props.has_type_params() {
                    let pred = syn::parse_str::<syn::WherePredicate>(&format!(
                        "&{} {}: ::std::default::Default",
                        (&new_lt).into_tokens(),
                        ty_inner,
                    )).expect("Failed to generate `WherePredicate`");
                    vec![pred]
                } else {
                    Vec::new()
                };
                let (generics, _) = extend_generics(generics, 0, &extra_preds);
                let (impl_generics, _, where_clause) = generics.split_for_impl();
                let helper_trait = props.helper_trait();
                quote! {
                    impl #impl_generics
                        ::std::default::Default for &#new_lt #ty_outer #type_generics
                    #where_clause
                    {
                        fn default() -> Self {
                            let inner = <&#new_lt #ty_inner as ::std::default::Default>::default();
                            <#ty_outer #type_generics as #helper_trait>::from_inner(inner)
                        }
                    }
                }
            },
            // `std::cmp::Partial{Eq,Ord}` traits.
            (Derive::PartialEqInner, _)
            | (Derive::PartialEqInnerRev, _)
            | (Derive::PartialEqInnerCow, Sizedness::Unsized)
            | (Derive::PartialEqInnerCowRev, Sizedness::Unsized)
            | (Derive::PartialEqSelf, _)
            | (Derive::PartialEqSelfCow, Sizedness::Unsized)
            | (Derive::PartialEqSelfCowRev, Sizedness::Unsized)
            | (Derive::PartialEqSelfCowAndInner, Sizedness::Unsized)
            | (Derive::PartialEqSelfCowAndInnerRev, Sizedness::Unsized)
            | (Derive::PartialOrdInner, _)
            | (Derive::PartialOrdInnerRev, _)
            | (Derive::PartialOrdInnerCow, Sizedness::Unsized)
            | (Derive::PartialOrdInnerCowRev, Sizedness::Unsized)
            | (Derive::PartialOrdSelf, _)
            | (Derive::PartialOrdSelfCow, Sizedness::Unsized)
            | (Derive::PartialOrdSelfCowRev, Sizedness::Unsized)
            | (Derive::PartialOrdSelfCowAndInner, Sizedness::Unsized)
            | (Derive::PartialOrdSelfCowAndInnerRev, Sizedness::Unsized) => {
                cmp::gen_impl_partial_cmp(*self, props)
            },
            (Derive::PartialEqInnerCow, Sizedness::Sized)
            | (Derive::PartialEqInnerCowRev, Sizedness::Sized)
            | (Derive::PartialEqSelfCow, Sizedness::Sized)
            | (Derive::PartialEqSelfCowRev, Sizedness::Sized)
            | (Derive::PartialEqSelfCowAndInner, Sizedness::Sized)
            | (Derive::PartialEqSelfCowAndInnerRev, Sizedness::Sized)
            | (Derive::PartialOrdInnerCow, Sizedness::Sized)
            | (Derive::PartialOrdInnerCowRev, Sizedness::Sized)
            | (Derive::PartialOrdSelfCow, Sizedness::Sized)
            | (Derive::PartialOrdSelfCowRev, Sizedness::Sized)
            | (Derive::PartialOrdSelfCowAndInner, Sizedness::Sized)
            | (Derive::PartialOrdSelfCowAndInnerRev, Sizedness::Sized) => panic!(
                "`#[opaque_typedef(derive({}))]` is not supported for sized types",
                self.as_ref()
            ),
            // `std::cmp::Ord` trait.
            (Derive::Ord, _) => cmp::gen_impl_ord(props),
            // `std::ascii::AsciiExt` trait.
            (Derive::AsciiExt, _) => {
                let ty_outer = &props.ty_outer;
                let type_generics = &props.type_generics;
                let ty_inner = props.field_inner.ty();
                let ty_inner_as_asciiext = quote!(<#ty_inner as ::std::ascii::AsciiExt>);
                let self_as_inner = props.tokens_outer_expr_as_inner(quote!(self));
                let other_as_inner = props.tokens_outer_expr_as_inner(quote!(other));
                if !props.is_mut_ref_allowed {
                    panic!(
                        "`#[opaque_typedef(derive({}))]` requires \
                         `#[opaque_typedef(allow_mut_ref)]`, but not specified",
                        self.as_ref()
                    );
                }
                let self_as_inner_mut = props.tokens_outer_expr_as_inner_mut(quote!(self));
                let extra_preds = if props.has_type_params() {
                    let ty_inner = ty_inner.into_tokens();
                    let pred = syn::parse_str::<syn::WherePredicate>(&format!(
                        "{}: ::std::ascii::AsciiExt",
                        ty_inner
                    )).expect("Failed to generate `WherePredicate`");
                    vec![pred]
                } else {
                    Vec::new()
                };
                let (generics, _) = extend_generics(Cow::Borrowed(props.generics), 0, &extra_preds);
                let (impl_generics, _, where_clause) = generics.split_for_impl();
                quote! {
                    impl #impl_generics ::std::ascii::AsciiExt for #ty_outer #type_generics
                    #where_clause
                    {
                        type Owned = #ty_inner_as_asciiext::Owned;
                        fn is_ascii(&self) -> bool {
                            #ty_inner_as_asciiext::is_ascii(#self_as_inner)
                        }
                        fn to_ascii_uppercase(&self) -> Self::Owned {
                            #ty_inner_as_asciiext::to_ascii_uppercase(#self_as_inner)
                        }
                        fn to_ascii_lowercase(&self) -> Self::Owned {
                            #ty_inner_as_asciiext::to_ascii_lowercase(#self_as_inner)
                        }
                        fn eq_ignore_ascii_case(&self, other: &Self) -> bool {
                            #ty_inner_as_asciiext::eq_ignore_ascii_case(
                                #self_as_inner,
                                #other_as_inner
                            )
                        }
                        fn make_ascii_uppercase(&mut self) {
                            #ty_inner_as_asciiext::make_ascii_uppercase(#self_as_inner_mut)
                        }
                        fn make_ascii_lowercase(&mut self) {
                            #ty_inner_as_asciiext::make_ascii_lowercase(#self_as_inner_mut)
                        }
                    }
                }
            },
            // Simple operators.
            _ => match OpSpec::from_derive_target(*self) {
                Some(op_spec) => match props.inner_sizedness {
                    Sizedness::Sized => op_spec.gen_impl_sized(props),
                    Sizedness::Unsized => op_spec.gen_impl_unsized(props),
                },
                None => {
                    panic!(
                        "Derive target {:?} is expected to be an operator, but \
                         lacks required properties",
                        *self
                    );
                },
            },
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
