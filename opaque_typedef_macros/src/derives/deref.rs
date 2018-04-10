//! Impl generators for `std::ops::Deref*` traits.

use quote;

use type_props::TypeProps;

use super::Derive;


/// Generates an impl for the target.
pub fn gen_impl(target: Derive, props: &TypeProps) -> quote::Tokens {
    let ty_outer = props.ty_outer;
    let impl_generics = &props.impl_generics;
    let type_generics = &props.type_generics;
    let where_clause = &props.where_clause;
    let ty_deref_target = props.tokens_ty_deref_target();

    match target {
        Derive::Deref => {
            let expr = gen_deref_expr(props);
            quote! {
                impl #impl_generics ::std::ops::Deref for #ty_outer #type_generics #where_clause {
                    type Target = #ty_deref_target;
                    fn deref(&self) -> &Self::Target {
                        #expr
                    }
                }
            }
        },
        Derive::DerefMut => {
            if !props.is_mut_ref_allowed {
                panic!(
                    "`#[opaque_typedef(derive({}))]` requires \
                     `#[opaque_typedef(allow_mut_ref)]`, but not specified",
                    target.as_ref()
                );
            }
            let expr = gen_deref_mut_expr(props);
            quote! {
                impl #impl_generics ::std::ops::DerefMut for #ty_outer #type_generics
                #where_clause
                {
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
    let ty_deref_target = props.tokens_ty_deref_target();
    let fn_name_deref = props.tokens_fn_deref();
    quote! {
        (#fn_name_deref(#self_as_inner) as &#ty_deref_target)
    }
}


pub fn gen_deref_mut_expr(props: &TypeProps) -> quote::Tokens {
    // The caller is responsible to ensure `allow_mut_ref` is specified.
    assert!(
        props.is_mut_ref_allowed,
        "opaque_typedef internal error: Caller should ensure `allow_mut_ref` is specified"
    );

    let self_as_inner_mut = props.tokens_outer_expr_as_inner_mut(quote!(self));
    let ty_deref_target = props.tokens_ty_deref_target();
    let fn_name_deref_mut = props.tokens_fn_deref_mut();
    quote! {
        (#fn_name_deref_mut(#self_as_inner_mut) as &mut #ty_deref_target)
    }
}
