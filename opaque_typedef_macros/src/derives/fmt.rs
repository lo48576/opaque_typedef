//! Impl generators for `std::fmt::*` traits.

use std::borrow::Cow;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn;

use type_props::TypeProps;
use utils::extend_generics;

use super::Derive;

/// Generates an impl for the target.
pub fn gen_impl(target: Derive, props: &TypeProps) -> TokenStream {
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
    let type_generics = &props.type_generics;
    let ty_inner = props.field_inner.ty();
    let self_as_inner = props.tokens_outer_expr_as_inner(quote!(self));
    let extra_preds = if props.has_type_params() {
        let ty_inner = ty_inner.into_token_stream();
        let pred = syn::parse_str::<syn::WherePredicate>(&format!(
            "{}: ::std::fmt::{}",
            ty_inner, trait_name,
        )).expect("Failed to generate `WherePredicate`");
        vec![pred]
    } else {
        Vec::new()
    };
    let (generics, _) = extend_generics(Cow::Borrowed(props.generics), 0, &extra_preds);
    let (impl_generics, _, where_clause) = generics.split_for_impl();
    quote! {
        impl #impl_generics ::std::fmt::#trait_name for #ty_outer #type_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                <#ty_inner as ::std::fmt::#trait_name>::fmt(#self_as_inner, f)
            }
        }
    }
}
