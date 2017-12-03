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
    static ref EMPTY_TOKENS: quote::Tokens = quote! {};
    static ref TOKEN_SELF: quote::Tokens = quote!(self);
    static ref TOKEN_OTHER: quote::Tokens = quote!(other);
}


/// Parameters used to implement `std::cmp::*` traits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplParams<'a, F> {
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

    pub fn impl_symmetric<'b, L: Into<Option<&'b quote::Tokens>>>(
        &self,
        cmp_target: CmpTarget,
        ty_lhs: &quote::Tokens,
        lhs_meta: (TypeWrap, InnerOrOuter),
        ty_rhs: &quote::Tokens,
        rhs_meta: (TypeWrap, InnerOrOuter),
        lifetimes: L,
    ) -> quote::Tokens {
        let lifetimes = lifetimes.into().unwrap_or(&*EMPTY_TOKENS);
        let inner_cmp_fn = match cmp_target {
            CmpTarget::PartialEq => self.inner_partial_eq_fn,
            CmpTarget::PartialOrd => self.inner_partial_ord_fn,
        };
        let lhs_to_inner = |expr| self.to_inner_ref(expr, lhs_meta);
        let rhs_to_inner = |expr| self.to_inner_ref(expr, rhs_meta);
        let impl0 = impl_single(
            cmp_target,
            inner_cmp_fn,
            ty_lhs,
            ty_rhs,
            &lhs_to_inner,
            &rhs_to_inner,
            lifetimes,
        );
        let impl1 = impl_single(
            cmp_target,
            inner_cmp_fn,
            ty_rhs,
            ty_lhs,
            &rhs_to_inner,
            &lhs_to_inner,
            lifetimes,
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
    /// `&SmartPtr<Target>` (such as `&Cow<Target>`).
    SmartPtrRef,
}

impl TypeWrap {
    pub fn to_ref<'a>(&self, expr: &'a quote::Tokens) -> Cow<'a, quote::Tokens> {
        match *self {
            TypeWrap::Ref => Cow::Borrowed(expr),
            TypeWrap::RefRef => Cow::Owned(quote!(*#expr)),
            TypeWrap::SmartPtrRef => Cow::Owned(quote!(&**#expr)),
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
