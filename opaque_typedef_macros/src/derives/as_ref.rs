//! Impl generators for `std::convert::As*` traits.

use proc_macro2::TokenStream;
use quote::ToTokens;

use type_props::TypeProps;

use super::deref::{gen_deref_expr, gen_deref_mut_expr};
use super::Derive;


/// Generates an impl for the target.
pub fn gen_impl(target: Derive, props: &TypeProps) -> TokenStream {
    let ty_outer = props.ty_outer;
    let impl_generics = &props.impl_generics;
    let type_generics = &props.type_generics;
    let where_clause = &props.where_clause;
    let ty_conv_target = match target {
        Derive::AsMutDeref | Derive::AsRefDeref => props.tokens_ty_deref_target(),
        Derive::AsMutInner | Derive::AsRefInner => props.field_inner.ty().into_token_stream(),
        Derive::AsMutSelf | Derive::AsRefSelf => quote!(#ty_outer #type_generics),
        _ => unreachable!("Should never happen"),
    };
    match target {
        Derive::AsMutDeref | Derive::AsMutInner | Derive::AsMutSelf => {
            if !props.is_mut_ref_allowed {
                panic!(
                    "`#[opaque_typedef(derive({}))]` requires \
                     `#[opaque_typedef(allow_mut_ref)]`, but not specified",
                    target.as_ref()
                );
            }
        },
        _ => {},
    }
    let expr = match target {
        Derive::AsMutDeref => gen_deref_mut_expr(props),
        Derive::AsMutInner => props.tokens_outer_expr_as_inner_mut(quote!(self)),
        Derive::AsMutSelf => quote!(self),
        Derive::AsRefDeref => gen_deref_expr(props),
        Derive::AsRefInner => props.tokens_outer_expr_as_inner(quote!(self)),
        Derive::AsRefSelf => quote!(self),
        _ => unreachable!("Should never happen"),
    };

    match target {
        Derive::AsMutDeref | Derive::AsMutInner | Derive::AsMutSelf => {
            quote! {
                impl #impl_generics
                    ::std::convert::AsMut<#ty_conv_target> for #ty_outer #type_generics
                #where_clause
                {
                    fn as_mut(&mut self) -> &mut #ty_conv_target {
                        #expr
                    }
                }
            }
        },
        Derive::AsRefDeref | Derive::AsRefInner | Derive::AsRefSelf => {
            quote! {
                impl #impl_generics
                    ::std::convert::AsRef<#ty_conv_target> for #ty_outer #type_generics
                #where_clause
                {
                    fn as_ref(&self) -> &#ty_conv_target {
                        #expr
                    }
                }
            }
        },
        _ => unreachable!("Should never happen"),
    }
}
