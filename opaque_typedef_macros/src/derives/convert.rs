//! Impl generators for `std::convert::{From,Inner}` traits.

use std::borrow::Cow;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn;

use type_props::{Sizedness, TypeProps};
use utils::extend_generics;

use super::Derive;

/// Generates an impl for the target.
pub fn gen_impl_from_inner(props: &TypeProps) -> TokenStream {
    let helper_trait = props.helper_trait();
    let ty_outer = props.ty_outer.into_token_stream();
    let ty_inner = props.field_inner.ty().into_token_stream();
    let type_generics = &props.type_generics;
    let expr = quote! {
        <#ty_outer #type_generics as #helper_trait>::from_inner(__inner)
    };
    match props.inner_sizedness {
        Sizedness::Sized => {
            let impl_generics = &props.impl_generics;
            let where_clause = &props.where_clause;
            quote! {
                impl #impl_generics ::std::convert::From<#ty_inner> for #ty_outer #type_generics
                #where_clause
                {
                    fn from(__inner: #ty_inner) -> Self {
                        #expr
                    }
                }
            }
        }
        Sizedness::Unsized => {
            let (generics, new_lifetimes) = extend_generics(Cow::Borrowed(props.generics), 1, &[]);
            let (impl_generics, _, where_clause) = generics.split_for_impl();
            let new_lt = &new_lifetimes[0];
            quote! {
                impl #impl_generics
                    ::std::convert::From<&#new_lt #ty_inner> for &#new_lt #ty_outer #type_generics
                #where_clause
                {
                    fn from(__inner: &#new_lt #ty_inner) -> Self {
                        #expr
                    }
                }
            }
        }
    }
}

/// Generates an impl for the target.
pub fn gen_impl_into_inner(props: &TypeProps) -> TokenStream {
    let helper_trait = props.helper_trait();
    let ty_outer = props.ty_outer.into_token_stream();
    let type_generics = &props.type_generics;
    let ty_inner = props.field_inner.ty().into_token_stream();
    match props.inner_sizedness {
        Sizedness::Sized => {
            let impl_generics = &props.impl_generics;
            let where_clause = &props.where_clause;
            quote! {
                impl #impl_generics ::std::convert::Into<#ty_inner> for #ty_outer #type_generics
                #where_clause
                {
                    fn into(self) -> #ty_inner {
                        <#ty_outer #type_generics as #helper_trait>::into_inner(self)
                    }
                }
            }
        }
        Sizedness::Unsized => {
            let (generics, new_lifetimes) = extend_generics(Cow::Borrowed(props.generics), 1, &[]);
            let (impl_generics, _, where_clause) = generics.split_for_impl();
            let new_lt = &new_lifetimes[0];
            quote! {
                impl #impl_generics
                    ::std::convert::Into<&#new_lt #ty_inner> for &#new_lt #ty_outer #type_generics
                #where_clause
                {
                    fn into(self) -> &#new_lt #ty_inner {
                        <#ty_outer #type_generics as #helper_trait>::as_inner(self)
                    }
                }
            }
        }
    }
}

/// Generates an impl for the target.
pub fn gen_impl_into_smartptr(target: Derive, props: &TypeProps) -> TokenStream {
    assert_eq!(
        props.inner_sizedness,
        Sizedness::Unsized,
        "opaque_typedef internal error: `gen_impl_into_smartptr()` works only for unsized types"
    );

    let ty_inner = props.field_inner.ty().into_token_stream();
    let ty_outer = props.ty_outer.into_token_stream();
    let type_generics = &props.type_generics;
    let (ty_smartptr_inner, ty_target, fn_to_inner_smartptr, fn_into_raw, fn_from_raw) =
        match target {
            Derive::IntoArc => (
                quote!(::std::sync::Arc<#ty_inner>),
                quote!(::std::sync::Arc<#ty_outer #type_generics>),
                quote!(::std::sync::Arc::<#ty_inner>::from),
                quote!(::std::sync::Arc::into_raw),
                quote!(::std::sync::Arc::from_raw),
            ),
            Derive::IntoBox => (
                quote!(::std::boxed::Box<#ty_inner>),
                quote!(::std::boxed::Box<#ty_outer #type_generics>),
                quote!(::std::boxed::Box::<#ty_inner>::from),
                quote!(::std::boxed::Box::into_raw),
                quote!(::std::boxed::Box::from_raw),
            ),
            Derive::IntoRc => (
                quote!(::std::rc::Rc<#ty_inner>),
                quote!(::std::rc::Rc<#ty_outer #type_generics>),
                quote!(::std::rc::Rc::<#ty_inner>::from),
                quote!(::std::rc::Rc::into_raw),
                quote!(::std::rc::Rc::from_raw),
            ),
            _ => unreachable!("Should never happen"),
        };
    let (generics, new_lt) = {
        let (temp_generics, mut new_lifetimes) =
            extend_generics(Cow::Borrowed(props.generics), 1, &[]);
        let new_lt = new_lifetimes.remove(0);
        let pred_clone = syn::parse_str::<syn::WherePredicate>(&format!(
            "{}: From<&{} {}>",
            &ty_smartptr_inner,
            (&new_lt).into_token_stream(),
            &ty_inner
        )).expect("Failed to generate `PredicateType`");
        let (generics, _) = extend_generics(temp_generics, 0, &[pred_clone]);
        (generics, new_lt)
    };
    let (impl_generics, _, where_clause) = generics.split_for_impl();

    // If the outer type has type parameter (i.e. the inner type can be any
    // type), implement `Into` trait instead of `From`.
    // This is because `From` can be problematic when impls such as
    // `impl From<MySlice<T>> for Box<[T]>` is generated.
    // (This doesn't compile: "error[E0210]: type parameter `T` must be used as
    // the type parameter for some local type (snip)".)
    // We can avoid this error by implementing `Into` instead, but then, of
    // course we can't use `From` impls (such as `Arc::<[T]>::from(my_slice)`).
    if props.has_type_params() {
        let self_as_inner = props.tokens_outer_expr_as_inner(quote!(self));
        quote! {
            impl #impl_generics
                ::std::convert::Into<#ty_target> for &#new_lt #ty_outer #type_generics
            #where_clause
            {
                fn into(self) -> #ty_target {
                    let smartptr_inner = #fn_to_inner_smartptr(#self_as_inner);
                    let raw = #fn_into_raw(smartptr_inner) as *mut #ty_outer #type_generics;
                    unsafe { #fn_from_raw(raw) }
                }
            }
        }
    } else {
        let other_as_inner = props.tokens_outer_expr_as_inner(quote!(__other));
        quote! {
            impl #impl_generics
                ::std::convert::From<&#new_lt #ty_outer #type_generics> for #ty_target
            #where_clause
            {
                fn from(__other: &#new_lt #ty_outer #type_generics) -> Self {
                    let smartptr_inner = #fn_to_inner_smartptr(#other_as_inner);
                    let raw = #fn_into_raw(smartptr_inner) as *mut #ty_outer #type_generics;
                    unsafe { #fn_from_raw(raw) }
                }
            }
        }
    }
}
