//! Utility functions and types.

use std::collections::HashMap;
use quote;
use syn;

use attrs;
use derives::Derive;
use fields;
use names;

pub mod impl_cmp;


/// Sizedness of the inner type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sizedness {
    /// Sized.
    Sized,
    /// Unsized.
    Unsized,
}

impl Sizedness {
    /// Returns true if the type is sized type, returns false otherwise.
    #[allow(dead_code)]
    pub fn is_sized(&self) -> bool {
        *self == Sizedness::Sized
    }

    /// Returns true if the type is unsized type, returns false otherwise.
    #[allow(dead_code)]
    pub fn is_unsized(&self) -> bool {
        *self == Sizedness::Unsized
    }
}


/// Deref-related specification.
#[derive(Debug, Clone, PartialEq, Eq)]
struct DerefSpec {
    /// A target type of `Deref`.
    target: quote::Tokens,
    /// Converter function for deref: `&Type -> &Target`.
    conv_deref: quote::Tokens,
    /// Converter function for mutable deref: `&mut Type -> &mut Target`.
    conv_deref_mut: Option<quote::Tokens>,
}

impl DerefSpec {
    /// Get deref spec from the given attributes.
    pub fn from_metaitems(metaitems: &[&syn::MetaItem]) -> Option<Self> {
        use syn::{MetaItem, NestedMetaItem};

        let props: HashMap<&str, &str> = metaitems
            .iter()
            .filter_map(|&meta| {
                if let MetaItem::List(ref ident, ref nested) = *meta {
                    if ident == names::DEREF {
                        return Some(nested);
                    }
                }
                None
            })
            .flat_map(|nested| nested)
            .filter_map(|nested| {
                match *nested {
                    // `deref(name = "value")` style.
                    NestedMetaItem::MetaItem(MetaItem::NameValue(
                        ref ident,
                        syn::Lit::Str(ref value, _style),
                    )) => Some((ident.as_ref(), value.as_str())),
                    _ => None,
                }
            })
            .collect();
        if props.is_empty() {
            return None;
        }
        let get_field = |name| {
            match props.get(name) {
                Some(v) => v,
                None => panic!(
                    "`#[opaque_typedef({}(..))]` is specified but `#[opaque_typedef({}({} = \"some_value\"))]` property was not found",
                    names::DEREF,
                    names::DEREF,
                    name
                ),
            }
        };
        let target = get_field(names::DEREF_TARGET);
        let conv_deref = get_field(names::DEREF_CONV);
        let conv_deref_mut = props.get(names::DEREF_CONV_MUT);
        let quote = |var| {
            let mut q = quote!{};
            q.append(var);
            q
        };
        Some(Self {
            target: quote(target),
            conv_deref: quote(conv_deref),
            conv_deref_mut: conv_deref_mut.map(quote),
        })
    }

    /// Returns the target type.
    pub fn target(&self) -> &quote::Tokens {
        &self.target
    }

    /// Returns the expression of dereferenced outer value.
    pub fn conv_deref(&self, expr_outer: &quote::Tokens) -> quote::Tokens {
        let conv = &self.conv_deref;
        quote! { #conv(#expr_outer) }
    }

    /// Returns the expression of mutably dereferenced outer value.
    pub fn conv_deref_mut(&self, expr_outer: &quote::Tokens) -> quote::Tokens {
        let conv = match self.conv_deref_mut {
            Some(ref v) => v,
            None => panic!(
                "`#[opaque_typedef({}({} = \"foo\"))]` property is required but not specified",
                names::DEREF,
                names::DEREF_CONV_MUT
            ),
        };
        quote! { #conv(#expr_outer) }
    }
}


/// Properties of the type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeProperties<'a> {
    /// Outer type.
    ty_outer: &'a syn::Ident,
    /// Inner type.
    ty_inner: &'a syn::Ty,
    /// Inner field name.
    field_inner: syn::Ident,
    /// Sizedness of the inner type.
    inner_sizedness: Sizedness,
    /// Traits to auto-derive.
    derives: Vec<Derive>,
    /// Deref spec.
    deref_spec: Option<DerefSpec>,
    /// Whether the use of mutable reference to inner value is allowed.
    mut_ref_allowed: bool,
}

impl<'a> TypeProperties<'a> {
    /// Creates a new `TypeProperties` from the given AST.
    pub fn from_ast(ast: &'a syn::DeriveInput, inner_sizedness: Sizedness) -> Self {
        let ty_outer = &ast.ident;
        if inner_sizedness.is_unsized() && !attrs::check_same_internal_repr(&ast.attrs) {
            panic!(
                concat!(
                    "To avoid undefined behavior, outer type `{}` should",
                    " be marked as `#[repr(C)]` or `#[repr(transparent)]`.\n",
                    "For detail, see <https://github.com/lo48576/opaque_typedef/issues/1>.\n",
                    "About `#[repr(transparent)]`, see RFC 1758",
                    " <https://github.com/rust-lang/rfcs/blob/master/text/1758-repr-transparent.md>."
                ),
                ty_outer.as_ref()
            );
        }
        let attrs = attrs::get_metaitems(&ast.attrs, names::ATTR_NAME);
        let (field_inner, ty_inner) = fields::get_inner_name_and_ty(ast);
        let derives = Derive::from_metaitems(&attrs);
        let deref_spec = DerefSpec::from_metaitems(&attrs);
        let mut_ref_allowed = attrs::has_word_prop(&attrs, names::ALLOW_MUT_REF_INNER);
        Self {
            ty_outer,
            ty_inner,
            field_inner,
            inner_sizedness,
            derives,
            deref_spec,
            mut_ref_allowed,
        }
    }

    /// Generates codes for the target type.
    pub fn impl_traits(&self) -> quote::Tokens {
        let mut tokens = quote!{};
        tokens.append(self.impl_basic_helper_trait());
        tokens.append(self.impl_auto_derive());
        tokens
    }

    /// Implements basic helper traits in `opaque_typedef` crate.
    pub fn impl_basic_helper_trait(&self) -> quote::Tokens {
        let ty_outer = self.ty_outer;
        let ty_inner = self.ty_inner;
        let field_inner = &self.field_inner;
        match self.inner_sizedness {
            Sizedness::Sized => {
                let validation_expr = quote! { inner };
                quote! {
                    impl ::opaque_typedef::OpaqueTypedef for #ty_outer {
                        type Inner = #ty_inner;
                        type Error = ::opaque_typedef::Infallible;

                        unsafe fn from_inner_unchecked(inner: Self::Inner) -> Self {
                            Self { #field_inner: inner }
                        }
                        fn from_inner(inner: Self::Inner) -> Result<Self, Self::Error> {
                            Ok(Self { #field_inner: #validation_expr })
                        }
                        fn into_inner(self) -> Self::Inner {
                            self.#field_inner
                        }
                        fn as_inner(&self) -> &Self::Inner {
                            &self.#field_inner
                        }
                        unsafe fn as_inner_mut(&mut self) -> &mut Self::Inner {
                            &mut self.#field_inner
                        }
                    }
                }
            },
            Sizedness::Unsized => {
                let validation_expr = quote!{};
                quote! {
                    impl ::opaque_typedef::OpaqueTypedefUnsized for #ty_outer {
                        type Inner = #ty_inner;
                        type Error = ::opaque_typedef::Infallible;

                        unsafe fn from_inner_unchecked(inner: &Self::Inner) -> &Self {
                            ::std::mem::transmute(inner)
                        }
                        unsafe fn from_inner_unchecked_mut(inner: &mut Self::Inner) -> &mut Self {
                            ::std::mem::transmute(inner)
                        }
                        fn from_inner(inner: &Self::Inner) -> Result<&Self, Self::Error> {
                            #validation_expr;
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked(inner) })
                        }
                        fn from_inner_mut(inner: &mut Self::Inner) -> Result<&mut Self, Self::Error> {
                            #validation_expr;
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked_mut(inner) })
                        }
                        fn as_inner(&self) -> &Self::Inner {
                            &self.#field_inner
                        }
                        unsafe fn as_inner_mut(&mut self) -> &mut Self::Inner {
                            &mut self.#field_inner
                        }
                    }
                }
            },
        }
    }

    /// Generates impls for auto-derive targets.
    pub fn impl_auto_derive(&self) -> quote::Tokens {
        let ty_outer = self.ty_outer;
        let ty_inner = self.ty_inner;
        let basic_trait = match self.inner_sizedness {
            Sizedness::Sized => quote! { ::opaque_typedef::OpaqueTypedef },
            Sizedness::Unsized => quote! { ::opaque_typedef::OpaqueTypedefUnsized },
        };
        let ty_deref_target = self.deref_spec
            .as_ref()
            .map(|spec| spec.target().clone())
            .unwrap_or(quote! { #ty_inner });
        let deref_conv = |var: &quote::Tokens| match self.deref_spec {
            Some(ref spec) => spec.conv_deref(var),
            None => quote! { <#ty_outer as #basic_trait>::as_inner(#var) },
        };
        let deref_mut_conv = |var: &quote::Tokens| match self.deref_spec {
            // Note that `spec.conv_deref_mut()` may panic.
            Some(ref spec) => spec.conv_deref_mut(var),
            None => quote! { unsafe { <#ty_outer as #basic_trait>::as_inner_mut(#var) } },
        };
        let self_deref = deref_conv(&quote!(self));
        // This may panic.
        let get_self_deref_mut = || deref_mut_conv(&quote!(self));
        let as_inner_conv =
            |var: &quote::Tokens| quote! { <#ty_outer as #basic_trait>::as_inner(#var) };
        let as_inner_mut_conv = |var: &quote::Tokens| {
            quote! { unsafe { <#ty_outer as #basic_trait>::as_inner_mut(#var) } }
        };
        // `self: &Outer` as `&Inner`.
        let self_as_inner = as_inner_conv(&quote!(self));
        // `self: &mut Outer` as `&mut Inner`.
        let self_as_inner_mut = as_inner_mut_conv(&quote!(self));
        // `other: &Outer` as `&Inner`.
        let other_as_inner = as_inner_conv(&quote!(other));
        let partial_eq_inner = quote! { <#ty_inner as ::std::cmp::PartialEq<#ty_inner>>::eq };
        let partial_ord_inner =
            quote! { <#ty_inner as ::std::cmp::PartialOrd<#ty_inner>>::partial_cmp };

        let mut tokens = quote!{};

        for &derive in &self.derives {
            if !self.mut_ref_allowed && derive.requires_mut_inner() {
                // The target trait equires `&mut self.#field_inner` but it is not allowed.
                panic!(
                    concat!(
                        "`#[opaque_typedef({}({}))]` requires inner value to be mutably referenced but it is not allowed.\n",
                        "To allow that, specify `#[opaque_typedef({})]`."
                    ),
                    names::DERIVE,
                    derive.as_ref(),
                    names::ALLOW_MUT_REF_INNER
                );
            }
            let impl_toks = match (derive, self.inner_sizedness) {
                (Derive::AsciiExt, _) => {
                    let ty_inner_as_asciiext = quote! { <#ty_inner as ::std::ascii::AsciiExt> };
                    quote! {
                        impl ::std::ascii::AsciiExt for #ty_outer {
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
                                #ty_inner_as_asciiext::eq_ignore_ascii_case(#self_as_inner, #other_as_inner)
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
                (Derive::AsMutDeref, _) => {
                    let self_deref_mut = get_self_deref_mut();
                    quote! {
                        impl<'a> ::std::convert::AsMut<#ty_deref_target> for #ty_outer {
                            fn as_mut(&mut self) -> &mut #ty_deref_target {
                                #self_deref_mut
                            }
                        }
                    }
                },
                (Derive::AsMutInner, _) => quote! {
                    impl ::std::convert::AsMut<#ty_inner> for #ty_outer {
                        fn as_mut(&mut self) -> &mut #ty_inner {
                            #self_as_inner_mut
                        }
                    }
                },
                (Derive::AsMutSelf, _) => quote! {
                    impl ::std::convert::AsMut<#ty_outer> for #ty_outer {
                        fn as_mut(&mut self) -> &mut Self {
                            self
                        }
                    }
                },
                (Derive::AsRefDeref, _) => quote! {
                    impl<'a> ::std::convert::AsRef<#ty_deref_target> for #ty_outer {
                        fn as_ref(&self) -> &#ty_deref_target {
                            #self_deref
                        }
                    }
                },
                (Derive::AsRefInner, _) => quote! {
                    impl ::std::convert::AsRef<#ty_inner> for #ty_outer {
                        fn as_ref(&self) -> &#ty_inner {
                            #self_as_inner
                        }
                    }
                },
                (Derive::AsRefSelf, _) => quote! {
                    impl ::std::convert::AsRef<#ty_outer> for #ty_outer {
                        fn as_ref(&self) -> &Self {
                            self
                        }
                    }
                },
                (Derive::DefaultRef, Sizedness::Unsized) => quote! {
                    impl<'a> ::std::default::Default for &'a #ty_outer {
                        fn default() -> Self {
                            let inner_default = <&'a #ty_inner as ::std::default::Default>::default();
                            let outer_res = <#ty_outer as #basic_trait>::from_inner(inner_default);
                            outer_res.unwrap()
                        }
                    }
                },
                (Derive::Deref, _) => quote! {
                    impl ::std::ops::Deref for #ty_outer {
                        type Target = #ty_deref_target;
                        fn deref(&self) -> &Self::Target {
                            #self_deref
                        }
                    }
                },
                (Derive::DerefMut, _) => {
                    let self_deref_mut = get_self_deref_mut();
                    quote! {
                        impl ::std::ops::DerefMut for #ty_outer {
                            fn deref_mut(&mut self) -> &mut Self::Target {
                                #self_deref_mut
                            }
                        }
                    }
                },
                (Derive::FromInner, Sizedness::Sized) => quote! {
                    impl ::std::convert::From<#ty_inner> for #ty_outer {
                        fn from(inner: #ty_inner) -> Self {
                            <#ty_outer as #basic_trait>::from_inner(inner).unwrap()
                        }
                    }
                },
                (Derive::FromInner, Sizedness::Unsized) => quote! {
                    impl<'a> ::std::convert::From<&'a #ty_inner> for &'a #ty_outer {
                        fn from(inner: &'a #ty_inner) -> Self {
                            <#ty_outer as #basic_trait>::from_inner(inner).unwrap()
                        }
                    }
                },
                (Derive::IntoArc, Sizedness::Unsized) => quote! {
                    impl<'a> ::std::convert::From<&'a #ty_outer> for ::std::sync::Arc<#ty_outer> {
                        fn from(other: &'a #ty_outer) -> Self {
                            let arc_inner = ::std::sync::Arc::<#ty_inner>::from(#other_as_inner);
                            let rw = ::std::sync::Arc::into_raw(arc_inner) as *mut #ty_outer;
                            unsafe { ::std::sync::Arc::from_raw(rw) }
                        }
                    }
                },
                (Derive::IntoBox, Sizedness::Unsized) => quote! {
                    impl<'a> ::std::convert::From<&'a #ty_outer> for ::std::boxed::Box<#ty_outer> {
                        fn from(other: &'a #ty_outer) -> Self {
                            let boxed_inner = ::std::boxed::Box::<#ty_inner>::from(#other_as_inner);
                            let rw = ::std::boxed::Box::into_raw(boxed_inner) as *mut #ty_outer;
                            unsafe { ::std::boxed::Box::from_raw(rw) }
                        }
                    }
                },
                (Derive::IntoRc, Sizedness::Unsized) => quote! {
                    impl<'a> ::std::convert::From<&'a #ty_outer> for ::std::rc::Rc<#ty_outer> {
                        fn from(other: &'a #ty_outer) -> Self {
                            let rc_inner = ::std::rc::Rc::<#ty_inner>::from(#other_as_inner);
                            let rw = ::std::rc::Rc::into_raw(rc_inner) as *mut #ty_outer;
                            unsafe { ::std::rc::Rc::from_raw(rw) }
                        }
                    }
                },
                (Derive::IntoInner, Sizedness::Sized) => quote! {
                    impl ::std::convert::Into<#ty_inner> for #ty_outer {
                        fn into(self) -> #ty_inner {
                            <#ty_outer as #basic_trait>::into_inner(self)
                        }
                    }
                },
                (Derive::IntoInner, Sizedness::Unsized) => quote! {
                    impl<'a> ::std::convert::Into<&'a #ty_inner> for &'a #ty_outer {
                        fn into(self) -> &'a #ty_inner {
                            <#ty_outer as #basic_trait>::as_inner(self)
                        }
                    }
                },
                // `std::cmp::*` stuff.
                (Derive::PartialEqInner, _)
                | (Derive::PartialEqInnerCow, Sizedness::Unsized)
                | (Derive::PartialEqSelfCow, Sizedness::Unsized)
                | (Derive::PartialEqSelfCowAndInner, Sizedness::Unsized)
                | (Derive::PartialOrdInner, _)
                | (Derive::PartialOrdInnerCow, Sizedness::Unsized)
                | (Derive::PartialOrdSelfCow, Sizedness::Unsized)
                | (Derive::PartialOrdSelfCowAndInner, Sizedness::Unsized) => {
                    use self::impl_cmp::InnerOrOuter::{Inner, Outer};
                    use self::impl_cmp::TypeWrap::{CowRef, Ref, RefRef};
                    let cmp_target = match derive {
                        Derive::PartialEqInner
                        | Derive::PartialEqInnerCow
                        | Derive::PartialEqSelfCow
                        | Derive::PartialEqSelfCowAndInner => impl_cmp::CmpTarget::PartialEq,
                        Derive::PartialOrdInner
                        | Derive::PartialOrdInnerCow
                        | Derive::PartialOrdSelfCow
                        | Derive::PartialOrdSelfCowAndInner => impl_cmp::CmpTarget::PartialOrd,
                        _ => unreachable!(),
                    };
                    let free_lifetimes = &["a", "b"];
                    let generator = impl_cmp::ImplParams {
                        ty_outer: &quote!(#ty_outer),
                        ty_inner: &quote!(#ty_inner),
                        outer_as_inner: &as_inner_conv,
                        inner_partial_eq_fn: &partial_eq_inner,
                        inner_partial_ord_fn: &partial_ord_inner,
                        free_lifetimes,
                    };
                    match derive {
                        Derive::PartialEqInner | Derive::PartialOrdInner => {
                            // `Inner` and `Outer`.
                            let inner_and_outer =
                                generator.impl_symmetric(cmp_target, (Ref, Inner), (Ref, Outer));
                            // `Inner` and `&Outer`.
                            let inner_and_outer_ref =
                                generator.impl_symmetric(cmp_target, (Ref, Inner), (RefRef, Outer));
                            // `&Inner` and `Outer`.
                            let inner_ref_and_outer =
                                generator.impl_symmetric(cmp_target, (RefRef, Inner), (Ref, Outer));
                            quote! {
                                #inner_and_outer
                                #inner_and_outer_ref
                                #inner_ref_and_outer
                            }
                        },
                        Derive::PartialEqInnerCow | Derive::PartialOrdInnerCow => {
                            // `Cow<Inner>` and `Outer`.
                            let inner_cow_and_outer =
                                generator.impl_symmetric(cmp_target, (CowRef, Inner), (Ref, Outer));
                            // `Cow<Inner>` and `&'b Outer`.
                            let inner_cow_and_outer_ref = generator.impl_symmetric(
                                cmp_target,
                                (CowRef, Inner),
                                (RefRef, Outer),
                            );
                            quote! {
                                #inner_cow_and_outer
                                #inner_cow_and_outer_ref
                            }
                        },
                        Derive::PartialEqSelfCow | Derive::PartialOrdSelfCow => {
                            // `Cow<Outer>` and `Outer`.
                            let outer_cow_and_outer =
                                generator.impl_symmetric(cmp_target, (CowRef, Outer), (Ref, Outer));
                            // `Cow<Outer>` and `&Outer`.
                            let outer_cow_and_outer_ref = generator.impl_symmetric(
                                cmp_target,
                                (CowRef, Outer),
                                (RefRef, Outer),
                            );
                            quote! {
                                #outer_cow_and_outer
                                #outer_cow_and_outer_ref
                            }
                        },
                        Derive::PartialEqSelfCowAndInner | Derive::PartialOrdSelfCowAndInner => {
                            // `Cow<Outer>` and `Inner`.
                            let outer_cow_and_inner =
                                generator.impl_symmetric(cmp_target, (CowRef, Outer), (Ref, Inner));
                            // `Cow<Outer>` and `&Inner`.
                            let outer_cow_and_inner_ref = generator.impl_symmetric(
                                cmp_target,
                                (CowRef, Outer),
                                (RefRef, Inner),
                            );
                            quote! {
                                #outer_cow_and_inner
                                #outer_cow_and_inner_ref
                            }
                        },
                        _ => unreachable!(),
                    }
                },
                // `std::fmt::*` stuff.
                (Derive::Binary, _)
                | (Derive::Display, _)
                | (Derive::LowerExp, _)
                | (Derive::LowerHex, _)
                | (Derive::Octal, _)
                | (Derive::Pointer, _)
                | (Derive::UpperExp, _)
                | (Derive::UpperHex, _) => {
                    let tr = {
                        let name = match derive {
                            Derive::Binary => "Binary",
                            Derive::Display => "Display",
                            Derive::LowerExp => "LowerExp",
                            Derive::LowerHex => "LowerHex",
                            Derive::Octal => "Octal",
                            Derive::Pointer => "Pointer",
                            Derive::UpperExp => "UpperExp",
                            Derive::UpperHex => "UpperHex",
                            _ => unreachable!(),
                        };
                        let mut tok = quote!{ ::std::fmt:: };
                        tok.append(name);
                        tok
                    };
                    quote! {
                        impl #tr for #ty_outer {
                            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                                <#ty_inner as #tr>::fmt(#self_as_inner, f)
                            }
                        }
                    }
                },
                (derive, sizedness) => {
                    let sizedness_str = match sizedness {
                        Sizedness::Sized => "sized",
                        Sizedness::Unsized => "unsized",
                    };
                    panic!(
                        "`#[opaque_typedef({}({}))]` is specified for `{}` but it is not supported for {} types",
                        names::DERIVE,
                        derive.as_ref(),
                        ty_outer,
                        sizedness_str
                    );
                },
            };
            tokens.append(impl_toks);
        }
        tokens
    }
}
