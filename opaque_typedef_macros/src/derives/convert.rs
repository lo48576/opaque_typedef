//! Impl generators for `std::convert::{From,Inner}` traits.

use quote;
use quote::ToTokens;

use type_props::{Sizedness, TypeProps};

use super::Derive;


/// Generates an impl for the target.
pub fn gen_impl_from_inner(props: &TypeProps) -> quote::Tokens {
    let helper_trait = props.helper_trait();
    let ty_outer = props.ty_outer.into_tokens();
    let ty_inner = props.field_inner.ty().into_tokens();
    let expr = quote! {
        <#ty_outer as #helper_trait>::from_inner(__inner).unwrap()
    };
    match props.inner_sizedness {
        Sizedness::Sized => {
            quote! {
                impl ::std::convert::From<#ty_inner> for #ty_outer {
                    fn from(__inner: #ty_inner) -> Self {
                        #expr
                    }
                }
            }
        },
        Sizedness::Unsized => {
            quote! {
                impl<'a> ::std::convert::From<&'a #ty_inner> for &'a #ty_outer {
                    fn from(__inner: &'a #ty_inner) -> Self {
                        #expr
                    }
                }
            }
        },
    }
}


/// Generates an impl for the target.
pub fn gen_impl_into_inner(props: &TypeProps) -> quote::Tokens {
    let helper_trait = props.helper_trait();
    let ty_outer = props.ty_outer.into_tokens();
    let ty_inner = props.field_inner.ty().into_tokens();
    match props.inner_sizedness {
        Sizedness::Sized => {
            quote! {
                impl ::std::convert::Into<#ty_inner> for #ty_outer {
                    fn into(self) -> #ty_inner {
                        <#ty_outer as #helper_trait>::into_inner(self)
                    }
                }
            }
        },
        Sizedness::Unsized => {
            quote! {
                impl<'a> ::std::convert::Into<&'a #ty_inner> for &'a #ty_outer {
                    fn into(self) -> &'a #ty_inner {
                        <#ty_outer as #helper_trait>::as_inner(self)
                    }
                }
            }
        },
    }
}


/// Generates an impl for the target.
pub fn gen_impl_into_smartptr(target: Derive, props: &TypeProps) -> quote::Tokens {
    assert_eq!(
        props.inner_sizedness,
        Sizedness::Unsized,
        "opaque_typedef internal error: `gen_impl_into_smartptr()` works only for unsized types"
    );

    let ty_outer = props.ty_outer.into_tokens();
    let ty_inner = props.field_inner.ty().into_tokens();
    let (ty_target, fn_to_inner_smartptr, fn_into_raw, fn_from_raw) = match target {
        Derive::IntoArc => (
            quote!(::std::sync::Arc<#ty_outer>),
            quote!(::std::sync::Arc::<#ty_inner>::from),
            quote!(::std::sync::Arc::into_raw),
            quote!(::std::sync::Arc::from_raw),
        ),
        Derive::IntoBox => (
            quote!(::std::boxed::Box<#ty_outer>),
            quote!(::std::boxed::Box::<#ty_inner>::from),
            quote!(::std::boxed::Box::into_raw),
            quote!(::std::boxed::Box::from_raw),
        ),
        Derive::IntoRc => (
            quote!(::std::rc::Rc<#ty_outer>),
            quote!(::std::rc::Rc::<#ty_inner>::from),
            quote!(::std::rc::Rc::into_raw),
            quote!(::std::rc::Rc::from_raw),
        ),
        _ => unreachable!("Should never happen"),
    };
    let other_as_inner = props.tokens_outer_expr_as_inner(quote!(__other));
    quote! {
        impl<'a> ::std::convert::From<&'a #ty_outer> for #ty_target {
            fn from(__other: &'a #ty_outer) -> Self {
                let smartptr_inner = #fn_to_inner_smartptr(#other_as_inner);
                let raw = #fn_into_raw(smartptr_inner) as *mut #ty_outer;
                unsafe { #fn_from_raw(raw) }
            }
        }
    }
}
