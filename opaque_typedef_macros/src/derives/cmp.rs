//! Impl generators for `std::cmp::Partial{Eq,Ord}` traits.

use quote;
use syn;

use type_props::TypeProps;

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
    // FIXME: Use `utils::extend_generics` to use extra lifetime (instead of
    // hard-coding "'__a").
    match target {
        Derive::PartialEqInner | Derive::PartialOrdInner => {
            let inner_and_outer = CmpImplSpec {
                type_props: props,
                extra_lifetimes: &[],
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(#ty_outer #type_generics),
                lhs_self_as_inner: &self_as_inner,
                lhs_other_as_inner: &other_as_inner,
                ty_rhs: ty_inner,
                rhs_self_as_inner: &quote!(self),
                rhs_other_as_inner: &quote!(other),
            }.gen_impl_symmetric();
            let extra_lifetimes = &[
                syn::parse_str::<syn::Lifetime>("'__a").expect("Should never fail"),
            ];
            let inner_and_outer_ref = CmpImplSpec {
                type_props: props,
                extra_lifetimes,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(&'__a #ty_outer #type_generics),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(*self)),
                lhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(*other)),
                ty_rhs: ty_inner,
                rhs_self_as_inner: &quote!(self),
                rhs_other_as_inner: &quote!(other),
            }.gen_impl_symmetric();
            let inner_ref_and_outer = CmpImplSpec {
                type_props: props,
                extra_lifetimes,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(#ty_outer #type_generics),
                lhs_self_as_inner: &self_as_inner,
                lhs_other_as_inner: &other_as_inner,
                ty_rhs: quote!(&'__a #ty_inner),
                rhs_self_as_inner: &quote!(*self),
                rhs_other_as_inner: &quote!(*other),
            }.gen_impl_symmetric();
            quote! {
                #inner_and_outer
                #inner_and_outer_ref
                #inner_ref_and_outer
            }
        },
        Derive::PartialEqInnerCow | Derive::PartialOrdInnerCow => {
            let extra_lifetimes = &[
                syn::parse_str::<syn::Lifetime>("'__a").expect("Should never fail"),
            ];
            let inner_cow_and_outer = CmpImplSpec {
                type_props: props,
                extra_lifetimes,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<'__a, #ty_inner>),
                lhs_self_as_inner: &quote!(&*self),
                lhs_other_as_inner: &quote!(&*other),
                ty_rhs: quote!(#ty_outer #type_generics),
                rhs_self_as_inner: &self_as_inner,
                rhs_other_as_inner: &other_as_inner,
            }.gen_impl_symmetric();
            let inner_cow_and_outer_ref = CmpImplSpec {
                type_props: props,
                extra_lifetimes,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<'__a, #ty_inner>),
                lhs_self_as_inner: &quote!(&*self),
                lhs_other_as_inner: &quote!(&*other),
                ty_rhs: quote!(&'__a #ty_outer #type_generics),
                rhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(*self)),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(*other)),
            }.gen_impl_symmetric();
            quote! {
                #inner_cow_and_outer
                #inner_cow_and_outer_ref
            }
        },
        Derive::PartialEqSelfCow | Derive::PartialOrdSelfCow => {
            let extra_lifetimes = &[
                syn::parse_str::<syn::Lifetime>("'__a").expect("Should never fail"),
            ];
            let outer_cow_and_outer = CmpImplSpec {
                type_props: props,
                extra_lifetimes,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<'__a, #ty_outer #type_generics>),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*self)),
                lhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*other)),
                ty_rhs: quote!(#ty_outer #type_generics),
                rhs_self_as_inner: &self_as_inner,
                rhs_other_as_inner: &other_as_inner,
            }.gen_impl_symmetric();
            let outer_cow_and_outer_ref = CmpImplSpec {
                type_props: props,
                extra_lifetimes,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<'__a, #ty_outer #type_generics>),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*self)),
                lhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*other)),
                ty_rhs: quote!(&'__a #ty_outer #type_generics),
                rhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(*self)),
                rhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(*other)),
            }.gen_impl_symmetric();
            quote! {
                #outer_cow_and_outer
                #outer_cow_and_outer_ref
            }
        },
        Derive::PartialEqSelfCowAndInner | Derive::PartialOrdSelfCowAndInner => {
            let extra_lifetimes = &[
                syn::parse_str::<syn::Lifetime>("'__a").expect("Should never fail"),
            ];
            let outer_cow_and_inner = CmpImplSpec {
                type_props: props,
                extra_lifetimes,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<'__a, #ty_outer #type_generics>),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*self)),
                lhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*other)),
                ty_rhs: ty_inner,
                rhs_self_as_inner: &quote!(self),
                rhs_other_as_inner: &quote!(other),
            }.gen_impl_symmetric();
            let outer_cow_and_inner_ref = CmpImplSpec {
                type_props: props,
                extra_lifetimes,
                trait_spec: trait_spec,
                ty_inner: ty_inner,
                ty_lhs: quote!(::std::borrow::Cow<'__a, #ty_outer #type_generics>),
                lhs_self_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*self)),
                lhs_other_as_inner: &props.tokens_outer_expr_as_inner(quote!(&*other)),
                ty_rhs: quote!(&'__a #ty_inner),
                rhs_self_as_inner: &quote!(*self),
                rhs_other_as_inner: &quote!(*other),
            }.gen_impl_symmetric();
            quote! {
                #outer_cow_and_inner
                #outer_cow_and_inner_ref
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


#[derive(Clone, Copy)]
struct CmpImplSpec<'a, TyI, TyL, TyR> {
    type_props: &'a TypeProps<'a>,
    extra_lifetimes: &'a [syn::Lifetime],
    trait_spec: CmpTraitSpec,
    ty_inner: TyI,
    ty_lhs: TyL,
    lhs_self_as_inner: &'a quote::Tokens,
    lhs_other_as_inner: &'a quote::Tokens,
    ty_rhs: TyR,
    rhs_self_as_inner: &'a quote::Tokens,
    rhs_other_as_inner: &'a quote::Tokens,
}

impl<'a, TyI, TyL, TyR> CmpImplSpec<'a, TyI, TyL, TyR>
where
    TyI: quote::ToTokens,
    TyL: quote::ToTokens,
    TyR: quote::ToTokens,
{
    fn gen_impl_symmetric(&self) -> quote::Tokens {
        let CmpImplSpec {
            type_props,
            extra_lifetimes,
            ref trait_spec,
            ref ty_inner,
            ref ty_lhs,
            lhs_self_as_inner,
            lhs_other_as_inner,
            ref ty_rhs,
            rhs_self_as_inner,
            rhs_other_as_inner,
        } = *self;
        let target_trait = trait_spec.target_trait();
        let method_name = trait_spec.method_name();
        let ty_ret = trait_spec.ty_ret();
        let mut old_generics = type_props.generics.clone();
        let generics = if extra_lifetimes.is_empty() {
            old_generics
        } else {
            extra_lifetimes.into_iter().for_each(|lt| {
                old_generics.params.insert(
                    0,
                    syn::GenericParam::Lifetime(syn::LifetimeDef::new(lt.clone())),
                )
            });
            old_generics
        };
        let (impl_generics, _type_generics, where_clause) = generics.split_for_impl();
        let fn_cmp = quote!(<#ty_inner as #target_trait<#ty_inner>>::#method_name);
        quote! {
            impl #impl_generics #target_trait<#ty_rhs> for #ty_lhs #where_clause {
                fn #method_name(&self, other: &#ty_rhs) -> #ty_ret {
                    #fn_cmp(#lhs_self_as_inner, #rhs_other_as_inner)
                }
            }
            impl #impl_generics #target_trait<#ty_lhs> for #ty_rhs #where_clause {
                fn #method_name(&self, other: &#ty_lhs) -> #ty_ret {
                    #fn_cmp(#rhs_self_as_inner, #lhs_other_as_inner)
                }
            }
        }
    }
}
