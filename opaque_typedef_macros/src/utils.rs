//! Utility functions and types.

use quote;
use syn;

use attrs;
use derives::Derive;
use fields;
use names;


/// Sizedness of the inner type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sizedness {
    /// Sized.
    Sized,
    /// Unsized.
    Unsized,
}

impl Sizedness {
    /// Returns true if the type is sized type, returns false otherwise.
    #[allow(dead_code)]
    pub fn is_sized(&self) -> bool {
        *self == Sizedness::Sized
    }

    /// Returns true if the type is unsized type, returns false otherwise.
    #[allow(dead_code)]
    pub fn is_unsized(&self) -> bool {
        *self == Sizedness::Unsized
    }
}


/// Properties of the type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeProperties<'a> {
    /// Outer type.
    ty_outer: &'a syn::Ident,
    /// Inner type.
    ty_inner: &'a syn::Ty,
    /// Inner field name.
    field_inner: syn::Ident,
    /// Sizedness of the inner type.
    inner_sizedness: Sizedness,
    /// Traits to auto-derive.
    derives: Vec<Derive>,
}

impl<'a> TypeProperties<'a> {
    /// Creates a new `TypeProperties` from the given AST.
    pub fn from_ast(ast: &'a syn::DeriveInput, inner_sizedness: Sizedness) -> Self {
        let ty_outer = &ast.ident;
        let attrs = attrs::get_metaitems(&ast.attrs, names::ATTR_NAME);
        let (field_inner, ty_inner) = fields::get_inner_name_and_ty(ast);
        let derives = Derive::from_metaitems(&attrs);
        Self {
            ty_outer,
            ty_inner,
            field_inner,
            inner_sizedness,
            derives,
        }
    }

    /// Generates codes for the target type.
    pub fn impl_traits(&self) -> quote::Tokens {
        let mut tokens = quote!{};
        tokens.append(self.impl_basic_helper_trait());
        tokens.append(self.impl_auto_derive());
        tokens
    }

    pub fn impl_basic_helper_trait(&self) -> quote::Tokens {
        let ty_outer = self.ty_outer;
        let ty_inner = self.ty_inner;
        let field_inner = &self.field_inner;
        match self.inner_sizedness {
            Sizedness::Sized => {
                let validation_expr = quote! { inner };
                quote! {
                    impl ::opaque_typedef::OpaqueTypedef for #ty_outer {
                        type Inner = #ty_inner;
                        type Error = ::opaque_typedef::Never;

                        unsafe fn from_inner_unchecked(inner: Self::Inner) -> Self {
                            Self { #field_inner: inner }
                        }
                        fn from_inner(inner: Self::Inner) -> Result<Self, Self::Error> {
                            Ok(Self { #field_inner: #validation_expr })
                        }
                        fn into_inner(self) -> Self::Inner {
                            self.#field_inner
                        }
                        fn as_inner(&self) -> &Self::Inner {
                            &self.#field_inner
                        }
                        unsafe fn as_inner_mut(&mut self) -> &mut Self::Inner {
                            &mut self.#field_inner
                        }
                    }
                }
            },
            Sizedness::Unsized => {
                let validation_expr = quote!{};
                quote! {
                    impl ::opaque_typedef::OpaqueTypedefUnsized for #ty_outer {
                        type Inner = #ty_inner;
                        type Error = ::opaque_typedef::Never;

                        unsafe fn from_inner_unchecked(inner: &Self::Inner) -> &Self {
                            ::std::mem::transmute(inner)
                        }
                        unsafe fn from_inner_unchecked_mut(inner: &mut Self::Inner) -> &mut Self {
                            ::std::mem::transmute(inner)
                        }
                        fn from_inner(inner: &Self::Inner) -> Result<&Self, Self::Error> {
                            #validation_expr;
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked(inner) })
                        }
                        fn from_inner_mut(inner: &mut Self::Inner) -> Result<&mut Self, Self::Error> {
                            #validation_expr;
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked_mut(inner) })
                        }
                        fn as_inner(&self) -> &Self::Inner {
                            &self.#field_inner
                        }
                        unsafe fn as_inner_mut(&mut self) -> &mut Self::Inner {
                            &mut self.#field_inner
                        }
                    }
                }
            },
        }
    }

    /// Generates impls for auto-derive targets.
    pub fn impl_auto_derive(&self) -> quote::Tokens {
        let ty_outer = self.ty_outer;
        let ty_inner = self.ty_inner;
        let basic_trait = match self.inner_sizedness {
            Sizedness::Sized => quote! { ::opaque_typedef::OpaqueTypedef },
            Sizedness::Unsized => quote! { ::opaque_typedef::OpaqueTypedefUnsized },
        };
        let ty_deref_target = quote! { #ty_inner };
        let deref_conv =
            |var: quote::Tokens| quote! { <#ty_outer as #basic_trait>::as_inner(#var) };
        let deref_mut_conv = |var: quote::Tokens| {
            quote! { unsafe { <#ty_outer as #basic_trait>::as_inner_mut(#var) } }
        };
        let self_deref = deref_conv(quote!(self));
        let self_deref_mut = deref_mut_conv(quote!(self));
        let as_inner_conv =
            |var: quote::Tokens| quote! { <#ty_outer as #basic_trait>::as_inner(#var) };
        let as_inner_mut_conv = |var: quote::Tokens| {
            quote! { unsafe { <#ty_outer as #basic_trait>::as_inner_mut(#var) } }
        };
        let self_as_inner = as_inner_conv(quote!(self));
        let self_as_inner_mut = as_inner_mut_conv(quote!(self));
        let mut tokens = quote!{};

        for &derive in &self.derives {
            let impl_toks = match (derive, self.inner_sizedness) {
                (Derive::AsRefInner, _) => quote! {
                    impl<'a> ::std::convert::AsRef<#ty_inner> for #ty_outer {
                        fn as_ref(&self) -> &#ty_inner {
                            #self_as_inner
                        }
                    }
                },
                (Derive::AsMutInner, _) => quote! {
                    impl<'a> ::std::convert::AsMut<#ty_inner> for #ty_outer {
                        fn as_mut(&mut self) -> &mut #ty_inner {
                            #self_as_inner_mut
                        }
                    }
                },
                (Derive::DefaultRef, Sizedness::Unsized) => quote! {
                    impl<'a> ::std::default::Default for &'a #ty_outer {
                        fn default() -> Self {
                            let inner_default = <&'a #ty_inner as ::std::default::Default>::default();
                            let outer_res = <#ty_outer as #basic_trait>::from_inner(inner_default);
                            outer_res.unwrap()
                        }
                    }
                },
                (Derive::Deref, _) => quote! {
                    impl ::std::ops::Deref for #ty_outer {
                        type Target = #ty_deref_target;
                        fn deref(&self) -> &Self::Target {
                            #self_deref
                        }
                    }
                },
                (Derive::DerefMut, _) => quote! {
                    impl ::std::ops::DerefMut for #ty_outer {
                        fn deref_mut(&mut self) -> &mut Self::Target {
                            #self_deref_mut
                        }
                    }
                },
                (derive, sizedness) => {
                    let sizedness_str = match sizedness {
                        Sizedness::Sized => "sized",
                        Sizedness::Unsized => "unsized",
                    };
                    panic!(
                        "`#[opaque_typedef({}({}))]` is specified for `{}` but it is not supported for {} types",
                        names::DERIVE,
                        derive.as_ref(),
                        ty_outer,
                        sizedness_str
                    );
                },
            };
            tokens.append(impl_toks);
        }
        tokens
    }
}
