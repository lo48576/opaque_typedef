//! Utility functions and types.

use quote;
use syn;

use attrs;
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
}

impl<'a> TypeProperties<'a> {
    /// Creates a new `TypeProperties` from the given AST.
    pub fn from_ast(ast: &'a syn::DeriveInput, inner_sizedness: Sizedness) -> Self {
        let ty_outer = &ast.ident;
        let attrs = attrs::get_metaitems(&ast.attrs, names::ATTR_NAME);
        let (field_inner, ty_inner) = fields::get_inner_name_and_ty(ast);
        Self {
            ty_outer,
            ty_inner,
            field_inner,
            inner_sizedness,
        }
    }

    /// Generates codes for the target type.
    pub fn impl_traits(&self) -> quote::Tokens {
        let mut tokens = quote!{};
        tokens.append(self.impl_basic_helper_trait());
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
}
