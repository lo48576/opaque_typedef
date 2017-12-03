//! Code generators to implement `std::cmp::*` traits.

use quote;


lazy_static! {
    static ref PARTIAL_EQ_TRAIT: quote::Tokens = quote!{ ::std::cmp::PartialEq };
    static ref PARTIAL_ORD_TRAIT: quote::Tokens = quote!{ ::std::cmp::PartialOrd };
    static ref PARTIAL_EQ_FN: quote::Tokens = quote!{ eq };
    static ref PARTIAL_ORD_FN: quote::Tokens = quote!{ partial_cmp };
    static ref PARTIAL_EQ_RET: quote::Tokens = quote!{ bool };
    static ref PARTIAL_ORD_RET: quote::Tokens = quote!{ ::std::option::Option<::std::cmp::Ordering> };
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


pub fn impl_single<Fl, Fr>(
    target: CmpTarget,
    inner_cmp_fn: &quote::Tokens,
    ty_lhs: &quote::Tokens,
    ty_rhs: &quote::Tokens,
    lhs_to_inner: Fl,
    rhs_to_inner: Fr,
    lifetimes: &quote::Tokens,
) -> quote::Tokens
where
    Fl: Fn(&quote::Tokens) -> quote::Tokens,
    Fr: Fn(&quote::Tokens) -> quote::Tokens,
{
    let cmp_trait = target.cmp_trait();
    let cmp_fn = target.cmp_fn();
    let ty_cmp_fn_ret = target.ty_cmp_fn_ret();
    let expr_self = lhs_to_inner(&quote!(self));
    let expr_other = rhs_to_inner(&quote!(other));
    quote! {
        impl #lifetimes #cmp_trait<#ty_rhs> for #ty_lhs {
            fn #cmp_fn(&self, other: &#ty_rhs) -> #ty_cmp_fn_ret {
                #inner_cmp_fn(#expr_self, #expr_other)
            }
        }
    }
}


pub fn impl_symmetric<Fl, Fr>(
    target: CmpTarget,
    inner_cmp_fn: &quote::Tokens,
    ty_lhs: &quote::Tokens,
    ty_rhs: &quote::Tokens,
    lhs_to_inner: Fl,
    rhs_to_inner: Fr,
    lifetimes: &quote::Tokens,
) -> quote::Tokens
where
    Fl: Fn(&quote::Tokens) -> quote::Tokens,
    Fr: Fn(&quote::Tokens) -> quote::Tokens,
{
    let impl0 = impl_single(
        target,
        inner_cmp_fn,
        ty_lhs,
        ty_rhs,
        &lhs_to_inner,
        &rhs_to_inner,
        lifetimes,
    );
    let impl1 = impl_single(
        target,
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
