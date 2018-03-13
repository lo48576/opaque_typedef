//! Impl generators for `std::cmp::Partial{Eq,Ord}` traits.

use std::borrow::Cow;

use quote;
use syn;

use type_props::TypeProps;
use utils::extend_generics;

use super::Derive;


/// Generates an impl for the target.
pub fn gen_impl_partial_cmp(target: Derive, props: &TypeProps) -> quote::Tokens {
    let _ = props;
    let trait_spec = match target {
        Derive::PartialEqInner
        | Derive::PartialEqInnerCow
        | Derive::PartialEqSelfCow
        | Derive::PartialEqSelfCowAndInner => CmpTraitSpec::PartialEq,
        Derive::PartialOrdInner
        | Derive::PartialOrdInnerCow
        | Derive::PartialOrdSelfCow
        | Derive::PartialOrdSelfCowAndInner => CmpTraitSpec::PartialOrd,
        _ => unreachable!("Should never happen"),
    };
    let ty_outer = &props.ty_outer;
    let type_generics = &props.type_generics;
    let ty_inner = props.field_inner.ty();
    let self_as_inner = props.tokens_outer_expr_as_inner(quote!(self));
    let other_as_inner = props.tokens_outer_expr_as_inner(quote!(other));
    match target {
        Derive::PartialEqInner | Derive::PartialOrdInner => {
            let inner_and_outer = CmpImplSpec {
                generics: props.generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(#ty_outer #type_generics),
                lhs_self_as_inner: &self_as_inner,
                ty_rhs: ty_inner,
                rhs_other_as_inner: &quote!(other),
            }.gen_impl();
            let inner_and_outer_rev = CmpImplSpec {
                generics: props.generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: ty_inner,
                lhs_self_as_inner: &quote!(self),
                ty_rhs: quote!(#ty_outer #type_generics),
                rhs_other_as_inner: &other_as_inner,
            }.gen_impl();
            let (generics, new_lts) = extend_generics(Cow::Borrowed(props.generics), 1, &[]);
            let new_lt = &new_lts[0];
            let inner_and_outer_ref = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(&#new_lt #ty_outer #type_generics),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(*self)),
                ty_rhs: ty_inner,
                rhs_other_as_inner: &quote!(other),
            }.gen_impl();
            let inner_and_outer_ref_rev = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: ty_inner,
                lhs_self_as_inner: &quote!(self),
                ty_rhs: quote!(&#new_lt #ty_outer #type_generics),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(*other)),
            }.gen_impl();
            let inner_ref_and_outer = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(#ty_outer #type_generics),
                lhs_self_as_inner: &self_as_inner,
                ty_rhs: quote!(&#new_lt #ty_inner),
                rhs_other_as_inner: &quote!(*other),
            }.gen_impl();
            let inner_ref_and_outer_rev = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(&#new_lt #ty_inner),
                lhs_self_as_inner: &quote!(*self),
                ty_rhs: quote!(#ty_outer #type_generics),
                rhs_other_as_inner: &other_as_inner,
            }.gen_impl();
            quote! {
                #inner_and_outer
                #inner_and_outer_rev
                #inner_and_outer_ref
                #inner_and_outer_ref_rev
                #inner_ref_and_outer
                #inner_ref_and_outer_rev
            }
        },
        Derive::PartialEqInnerCow | Derive::PartialOrdInnerCow => {
            let (generics, new_lts) = extend_generics(Cow::Borrowed(props.generics), 1, &[]);
            let new_lt = &new_lts[0];
            let inner_cow_and_outer = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<#new_lt, #ty_inner>),
                lhs_self_as_inner: &quote!(&*self),
                ty_rhs: quote!(#ty_outer #type_generics),
                rhs_other_as_inner: &other_as_inner,
            }.gen_impl();
            let inner_cow_and_outer_rev = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(#ty_outer #type_generics),
                lhs_self_as_inner: &self_as_inner,
                ty_rhs: quote!(::std::borrow::Cow<#new_lt, #ty_inner>),
                rhs_other_as_inner: &quote!(&*other),
            }.gen_impl();
            let (generics, new_lts) = extend_generics(Cow::Borrowed(props.generics), 2, &[]);
            let new_lt0 = &new_lts[0];
            let new_lt1 = &new_lts[1];
            let inner_cow_and_outer_ref = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<#new_lt0, #ty_inner>),
                lhs_self_as_inner: &quote!(&*self),
                ty_rhs: quote!(&#new_lt1 #ty_outer #type_generics),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(*other)),
            }.gen_impl();
            let inner_cow_and_outer_ref_rev = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(&#new_lt0 #ty_outer #type_generics),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(*self)),
                ty_rhs: quote!(::std::borrow::Cow<#new_lt1, #ty_inner>),
                rhs_other_as_inner: &quote!(&*other),
            }.gen_impl();
            quote! {
                #inner_cow_and_outer
                #inner_cow_and_outer_rev
                #inner_cow_and_outer_ref
                #inner_cow_and_outer_ref_rev
            }
        },
        Derive::PartialEqSelfCow | Derive::PartialOrdSelfCow => {
            let (generics, new_lts) = extend_generics(Cow::Borrowed(props.generics), 1, &[]);
            let new_lt = &new_lts[0];
            let outer_cow_and_outer = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<#new_lt, #ty_outer #type_generics>),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*self)),
                ty_rhs: quote!(#ty_outer #type_generics),
                rhs_other_as_inner: &other_as_inner,
            }.gen_impl();
            let outer_cow_and_outer_rev = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(#ty_outer #type_generics),
                lhs_self_as_inner: &self_as_inner,
                ty_rhs: quote!(::std::borrow::Cow<#new_lt, #ty_outer #type_generics>),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*other)),
            }.gen_impl();
            let (generics, new_lts) = extend_generics(Cow::Borrowed(props.generics), 2, &[]);
            let new_lt0 = &new_lts[0];
            let new_lt1 = &new_lts[1];
            let outer_cow_and_outer_ref = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<#new_lt0, #ty_outer #type_generics>),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*self)),
                ty_rhs: quote!(&#new_lt1 #ty_outer #type_generics),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(*other)),
            }.gen_impl();
            let outer_cow_and_outer_ref_rev = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(&#new_lt0 #ty_outer #type_generics),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(*self)),
                ty_rhs: quote!(::std::borrow::Cow<#new_lt1, #ty_outer #type_generics>),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*other)),
            }.gen_impl();
            quote! {
                #outer_cow_and_outer
                #outer_cow_and_outer_rev
                #outer_cow_and_outer_ref
                #outer_cow_and_outer_ref_rev
            }
        },
        Derive::PartialEqSelfCowAndInner | Derive::PartialOrdSelfCowAndInner => {
            let (generics, new_lts) = extend_generics(Cow::Borrowed(props.generics), 1, &[]);
            let new_lt = &new_lts[0];
            let outer_cow_and_inner = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<#new_lt, #ty_outer #type_generics>),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*self)),
                ty_rhs: ty_inner,
                rhs_other_as_inner: &quote!(other),
            }.gen_impl();
            let outer_cow_and_inner_rev = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: ty_inner,
                lhs_self_as_inner: &quote!(self),
                ty_rhs: quote!(::std::borrow::Cow<#new_lt, #ty_outer #type_generics>),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*other)),
            }.gen_impl();
            let (generics, new_lts) = extend_generics(Cow::Borrowed(props.generics), 2, &[]);
            let new_lt0 = &new_lts[0];
            let new_lt1 = &new_lts[1];
            let outer_cow_and_inner_ref = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<#new_lt0, #ty_outer #type_generics>),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*self)),
                ty_rhs: quote!(&#new_lt1 #ty_inner),
                rhs_other_as_inner: &quote!(*other),
            }.gen_impl();
            let outer_cow_and_inner_ref_rev = CmpImplSpec {
                generics: &generics,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(&#new_lt0 #ty_inner),
                lhs_self_as_inner: &quote!(*self),
                ty_rhs: quote!(::std::borrow::Cow<#new_lt1, #ty_outer #type_generics>),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*other)),
            }.gen_impl();
            quote! {
                #outer_cow_and_inner
                #outer_cow_and_inner_rev
                #outer_cow_and_inner_ref
                #outer_cow_and_inner_ref_rev
            }
        },
        _ => unreachable!("Should never happen"),
    }
}


#[derive(Debug, Clone, Copy)]
enum CmpTraitSpec {
    PartialEq,
    PartialOrd,
}

impl CmpTraitSpec {
    pub fn target_trait(&self) -> quote::Tokens {
        match *self {
            CmpTraitSpec::PartialEq => quote!(::std::cmp::PartialEq),
            CmpTraitSpec::PartialOrd => quote!(::std::cmp::PartialOrd),
        }
    }

    pub fn method_name(&self) -> quote::Tokens {
        match *self {
            CmpTraitSpec::PartialEq => quote!(eq),
            CmpTraitSpec::PartialOrd => quote!(partial_cmp),
        }
    }

    pub fn ty_ret(&self) -> quote::Tokens {
        match *self {
            CmpTraitSpec::PartialEq => quote!(bool),
            CmpTraitSpec::PartialOrd => quote!(Option<::std::cmp::Ordering>),
        }
    }
}


#[derive(Clone)]
struct CmpImplSpec<'a, TyI, TyL, TyR> {
    generics: &'a syn::Generics,
    trait_spec: CmpTraitSpec,
    ty_inner: TyI,
    ty_lhs: TyL,
    lhs_self_as_inner: &'a quote::Tokens,
    ty_rhs: TyR,
    rhs_other_as_inner: &'a quote::Tokens,
}

impl<'a, TyI, TyL, TyR> CmpImplSpec<'a, TyI, TyL, TyR>
where
    TyI: quote::ToTokens,
    TyL: quote::ToTokens,
    TyR: quote::ToTokens,
{
    fn gen_impl(&self) -> quote::Tokens {
        let CmpImplSpec {
            generics,
            ref trait_spec,
            ref ty_inner,
            ref ty_lhs,
            lhs_self_as_inner,
            ref ty_rhs,
            rhs_other_as_inner,
        } = *self;
        let target_trait = trait_spec.target_trait();
        let method_name = trait_spec.method_name();
        let ty_ret = trait_spec.ty_ret();
        let (impl_generics, _, where_clause) = generics.split_for_impl();
        let fn_cmp = quote!(<#ty_inner as #target_trait<#ty_inner>>::#method_name);
        quote! {
            impl #impl_generics #target_trait<#ty_rhs> for #ty_lhs #where_clause {
                fn #method_name(&self, other: &#ty_rhs) -> #ty_ret {
                    #fn_cmp(#lhs_self_as_inner, #rhs_other_as_inner)
                }
            }
        }
    }
}
