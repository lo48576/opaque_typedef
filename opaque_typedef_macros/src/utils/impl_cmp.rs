//! Code generators to implement `std::cmp::*` traits.

use std::borrow::Cow;
use quote;


lazy_static! {
    static ref PARTIAL_EQ_TRAIT: quote::Tokens = quote!{ ::std::cmp::PartialEq };
    static ref PARTIAL_ORD_TRAIT: quote::Tokens = quote!{ ::std::cmp::PartialOrd };
    static ref PARTIAL_EQ_FN: quote::Tokens = quote!{ eq };
    static ref PARTIAL_ORD_FN: quote::Tokens = quote!{ partial_cmp };
    static ref PARTIAL_EQ_RET: quote::Tokens = quote!{ bool };
    static ref PARTIAL_ORD_RET: quote::Tokens = quote!{ ::std::option::Option<::std::cmp::Ordering> };
    static ref TOKEN_SELF: quote::Tokens = quote!(self);
    static ref TOKEN_OTHER: quote::Tokens = quote!(other);
}


/// Parameters used to implement `std::cmp::*` traits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplParams<'a, F> {
    pub ty_outer: &'a quote::Tokens,
    pub ty_inner: &'a quote::Tokens,
    pub outer_as_inner: F,
    pub inner_partial_eq_fn: &'a quote::Tokens,
    pub inner_partial_ord_fn: &'a quote::Tokens,
}

impl<'a, F> ImplParams<'a, F>
where
    F: Fn(&quote::Tokens) -> quote::Tokens,
{
    pub fn to_inner_ref(
        &self,
        expr: &quote::Tokens,
        type_meta: (TypeWrap, InnerOrOuter),
    ) -> quote::Tokens {
        let (wrap, in_or_out) = type_meta;
        // `&Inner` or `Outer`.
        let expr_ref = wrap.to_ref(expr);
        // `&Inner`.
        match in_or_out {
            InnerOrOuter::Inner => expr_ref.into_owned(),
            InnerOrOuter::Outer => (self.outer_as_inner)(&*expr_ref),
        }
    }

    pub fn base_type(&self, in_or_out: InnerOrOuter) -> &'a quote::Tokens {
        match in_or_out {
            InnerOrOuter::Inner => self.ty_inner,
            InnerOrOuter::Outer => self.ty_outer,
        }
    }

    pub fn impl_symmetric(
        &self,
        cmp_target: CmpTarget,
        lhs_meta: (TypeWrap, InnerOrOuter),
        rhs_meta: (TypeWrap, InnerOrOuter),
    ) -> quote::Tokens {
        let inner_cmp_fn = match cmp_target {
            CmpTarget::PartialEq => self.inner_partial_eq_fn,
            CmpTarget::PartialOrd => self.inner_partial_ord_fn,
        };
        let ty_lhs_base = self.base_type(lhs_meta.1);
        let ty_rhs_base = self.base_type(rhs_meta.1);
        let ty_lhs_wrapped_unref = lhs_meta.0.ty_wrapped_unref(ty_lhs_base, Some(&quote!('a)));
        let ty_rhs_wrapped_unref = rhs_meta.0.ty_wrapped_unref(ty_rhs_base, Some(&quote!('b)));
        let lhs_to_inner = |expr| self.to_inner_ref(expr, lhs_meta);
        let rhs_to_inner = |expr| self.to_inner_ref(expr, rhs_meta);
        let impl0 = impl_single(
            cmp_target,
            inner_cmp_fn,
            &ty_lhs_wrapped_unref,
            &ty_rhs_wrapped_unref,
            &lhs_to_inner,
            &rhs_to_inner,
            &quote! { <'a, 'b> },
        );
        let impl1 = impl_single(
            cmp_target,
            inner_cmp_fn,
            &ty_rhs_wrapped_unref,
            &ty_lhs_wrapped_unref,
            &rhs_to_inner,
            &lhs_to_inner,
            &quote! { <'a, 'b> },
        );
        quote! {
            #impl0
            #impl1
        }
    }
}


/// A wrapping of a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeWrap {
    /// `&Target`.
    Ref,
    /// `&&Target`.
    RefRef,
    /// `&Cow<Target>`.
    CowRef,
}

impl TypeWrap {
    pub fn to_ref<'a>(&self, expr: &'a quote::Tokens) -> Cow<'a, quote::Tokens> {
        match *self {
            TypeWrap::Ref => Cow::Borrowed(expr),
            TypeWrap::RefRef => Cow::Owned(quote!(*#expr)),
            TypeWrap::CowRef => Cow::Owned(quote!(&**#expr)),
        }
    }

    // `lifetime` should contain leading `'`.
    pub fn ty_wrapped_unref<'a>(
        &self,
        ty_base: &'a quote::Tokens,
        lifetime: Option<&quote::Tokens>,
    ) -> Cow<'a, quote::Tokens> {
        match *self {
            TypeWrap::Ref => Cow::Borrowed(ty_base),
            TypeWrap::RefRef => {
                let lt = lifetime.expect("Lifetitme is required for `TypeWrap::RefRef`");
                Cow::Owned(quote! { &#lt #ty_base })
            },
            TypeWrap::CowRef => {
                let lt = lifetime.expect("Lifetitme is required for `TypeWrap::CowRef`");
                Cow::Owned(quote! { ::std::borrow::Cow<#lt, #ty_base> })
            },
        }
    }
}


/// Inner or outer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InnerOrOuter {
    /// Inner.
    Inner,
    /// Outer.
    Outer,
}


/// Compare target trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CmpTarget {
    /// `PartialEq`.
    PartialEq,
    /// `PartialOrd`.
    PartialOrd,
}

impl CmpTarget {
    fn cmp_trait(&self) -> &'static quote::Tokens {
        match *self {
            CmpTarget::PartialEq => &*PARTIAL_EQ_TRAIT,
            CmpTarget::PartialOrd => &*PARTIAL_ORD_TRAIT,
        }
    }

    fn cmp_fn(&self) -> &'static quote::Tokens {
        match *self {
            CmpTarget::PartialEq => &*PARTIAL_EQ_FN,
            CmpTarget::PartialOrd => &*PARTIAL_ORD_FN,
        }
    }

    fn ty_cmp_fn_ret(&self) -> &'static quote::Tokens {
        match *self {
            CmpTarget::PartialEq => &*PARTIAL_EQ_RET,
            CmpTarget::PartialOrd => &*PARTIAL_ORD_RET,
        }
    }
}


pub fn impl_single<'l, 'r, Fl, Fr>(
    target: CmpTarget,
    inner_cmp_fn: &quote::Tokens,
    ty_lhs: &quote::Tokens,
    ty_rhs: &quote::Tokens,
    lhs_to_inner: Fl,
    rhs_to_inner: Fr,
    lifetimes: &quote::Tokens,
) -> quote::Tokens
where
    Fl: Fn(&'l quote::Tokens) -> quote::Tokens,
    Fr: Fn(&'r quote::Tokens) -> quote::Tokens,
{
    let cmp_trait = target.cmp_trait();
    let cmp_fn = target.cmp_fn();
    let ty_cmp_fn_ret = target.ty_cmp_fn_ret();
    let expr_self = lhs_to_inner(&*TOKEN_SELF);
    let expr_other = rhs_to_inner(&*TOKEN_OTHER);
    quote! {
        impl #lifetimes #cmp_trait<#ty_rhs> for #ty_lhs {
            fn #cmp_fn(&self, other: &#ty_rhs) -> #ty_cmp_fn_ret {
                #inner_cmp_fn(#expr_self, #expr_other)
            }
        }
    }
}
