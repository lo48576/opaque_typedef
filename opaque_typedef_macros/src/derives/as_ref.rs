//! Impl generators for `std::convert::As*` traits.

use quote;
use quote::ToTokens;

use type_props::TypeProps;

use super::Derive;
use super::deref::{gen_deref_expr, gen_deref_mut_expr};


/// Generates an impl for the target.
pub fn gen_impl(target: Derive, props: &TypeProps) -> quote::Tokens {
    let ty_outer = props.ty_outer;
    let ty_conv_target = match target {
        Derive::AsMutDeref | Derive::AsRefDeref => props.tokens_ty_deref_target(),
        Derive::AsMutInner | Derive::AsRefInner => props.field_inner.ty().into_tokens(),
        Derive::AsMutSelf | Derive::AsRefSelf => ty_outer.into_tokens(),
        _ => unreachable!("Should never happen"),
    };
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
                impl ::std::convert::AsMut<#ty_conv_target> for #ty_outer {
                    fn as_mut(&mut self) -> &mut #ty_conv_target {
                        #expr
                    }
                }
            }
        },
        Derive::AsRefDeref | Derive::AsRefInner | Derive::AsRefSelf => {
            quote! {
                impl ::std::convert::AsRef<#ty_conv_target> for #ty_outer {
                    fn as_ref(&self) -> &#ty_conv_target {
                        #expr
                    }
                }
            }
        },
        _ => unreachable!("Should never happen"),
    }
}
