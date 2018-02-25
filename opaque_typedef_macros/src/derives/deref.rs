//! Impl generators for `std::ops::Deref*` traits.

use quote;

use type_props::TypeProps;

use super::Derive;


/// Generates an impl for the target.
pub fn gen_impl(target: Derive, props: &TypeProps) -> quote::Tokens {
    let ty_outer = props.ty_outer;
    let (_, ty_deref_target) = props.tokens_fn_deref_and_ty_deref_target();

    match target {
        Derive::Deref => {
            let expr = gen_deref_expr(props);
            quote! {
                impl ::std::ops::Deref for #ty_outer {
                    type Target = #ty_deref_target;
                    fn deref(&self) -> &Self::Target {
                        #expr
                    }
                }
            }
        },
        Derive::DerefMut => {
            let expr = gen_deref_mut_expr(props);
            quote! {
                impl ::std::ops::DerefMut for #ty_outer {
                    fn deref_mut(&mut self) -> &mut Self::Target {
                        #expr
                    }
                }
            }
        },
        _ => unreachable!(
            "Should never happen: `derives::deref::gen_impl` got `{}` target",
            target.as_ref()
        ),
    }
}


pub fn gen_deref_expr(props: &TypeProps) -> quote::Tokens {
    let self_as_inner = props.tokens_outer_expr_as_inner(quote!(self));
    let (fn_name_deref, ty_deref_target) = props.tokens_fn_deref_and_ty_deref_target();
    quote! {
        (#fn_name_deref(#self_as_inner) as &#ty_deref_target)
    }
}


pub fn gen_deref_mut_expr(props: &TypeProps) -> quote::Tokens {
    let self_as_inner_mut = props.tokens_outer_expr_as_inner_mut(quote!(self));
    let (fn_name_deref_mut, ty_deref_target) = props.tokens_fn_deref_mut_and_ty_deref_target();
    quote! {
        (#fn_name_deref_mut(#self_as_inner_mut) as &mut #ty_deref_target)
    }
}
