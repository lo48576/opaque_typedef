//! Impl generators for `std::fmt::*` traits.

use quote;

use type_props::TypeProps;

use super::Derive;


/// Generates an impl for the target.
pub fn gen_impl(target: Derive, props: &TypeProps) -> quote::Tokens {
    let trait_name = match target {
        Derive::Binary => quote!(Binary),
        Derive::Display => quote!(Display),
        Derive::LowerExp => quote!(LowerExp),
        Derive::LowerHex => quote!(LowerHex),
        Derive::Octal => quote!(Octal),
        Derive::Pointer => quote!(Pointer),
        Derive::UpperExp => quote!(UpperExp),
        Derive::UpperHex => quote!(UpperHex),
        _ => unreachable!("Should never happen"),
    };
    let ty_outer = props.ty_outer;
    let impl_generics = &props.impl_generics;
    let type_generics = &props.type_generics;
    let where_clause = &props.where_clause;
    let ty_inner = props.field_inner.ty();
    let self_as_inner = props.tokens_outer_expr_as_inner(quote!(self));
    quote! {
        impl #impl_generics ::std::fmt::#trait_name for #ty_outer #type_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                <#ty_inner as ::std::fmt::#trait_name>::fmt(#self_as_inner, f)
            }
        }
    }
}
