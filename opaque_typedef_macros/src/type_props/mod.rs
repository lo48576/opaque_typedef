//! Type properties.

use quote;
use quote::ToTokens;
use syn;
use syn::DeriveInput;

use derives::Derive;

use self::builder::TypePropsBuilder;

mod builder;


/// Sizedness of a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sizedness {
    /// Sized.
    Sized,
    /// Unsized.
    Unsized,
}


/// A field with the optional index.
#[derive(Clone)]
pub enum Field<'a> {
    /// A named field.
    Named(&'a syn::Field),
    /// An unnamed field with its index.
    Unnamed(&'a syn::Field, usize),
}

impl<'a> Field<'a> {
    /// Returns the type of the field.
    pub fn ty(&self) -> &'a syn::Type {
        match *self {
            Field::Named(field) => &field.ty,
            Field::Unnamed(field, _) => &field.ty,
        }
    }

    /// Returns the name of the field.
    pub fn name(&self) -> quote::Tokens {
        match *self {
            Field::Named(field) => field
                .ident
                .as_ref()
                .expect("Should never happen")
                .into_tokens(),
            Field::Unnamed(_, index) => syn::parse_str::<syn::LitInt>(&format!("{}", index))
                .expect("Should never happen")
                .into_tokens(),
        }
    }
}


#[derive(Clone)]
pub struct DerefSpec {
    /// Deref target type.
    pub ty_deref_target: syn::Type,
    /// Converter function from inner type to target type.
    ///
    /// The function should have `&Inner -> &Target` type.
    pub fn_name_deref: syn::Expr,
    /// Converter function from inner type to target type.
    ///
    /// The function should have `&Inner -> &Target` type.
    pub fn_name_deref_mut: Option<syn::Expr>,
}


/// Properties of a type with `#[derive(OpaqueTypedef*)]`.
#[derive(Clone)]
pub struct TypeProps<'a> {
    /// Outer type.
    pub ty_outer: &'a syn::Ident,
    /// Inner field.
    pub field_inner: Field<'a>,
    /// Sizedness of the inner type.
    pub inner_sizedness: Sizedness,
    /// Derive target traits.
    pub derives: Vec<Derive>,
    /// Deref spec.
    pub deref_spec: Option<DerefSpec>,
}

impl<'a> TypeProps<'a> {
    /// Load properties from the given input and sizedness.
    pub fn load(input: &'a DeriveInput, sizedness: Sizedness) -> Self {
        let mut builder = TypePropsBuilder::new();
        builder.load(input, sizedness);
        builder.build()
    }

    /// Generates implementations for the target type.
    pub fn gen_impls(&self) -> quote::Tokens {
        let basic_impl = self.impl_basic_helper_trait();
        let derive_impls = self.derives
            .iter()
            .map(|derive| derive.impl_auto_derive(self))
            .collect::<Vec<_>>();
        quote! {
            #basic_impl
            #(#derive_impls)*
        }
    }

    /// Generates impl for `OpaqueTypedef*` trait.
    pub fn impl_basic_helper_trait(&self) -> quote::Tokens {
        let ty_outer = self.ty_outer;
        let ty_inner = self.field_inner.ty();
        let name_inner = self.field_inner.name();
        match self.inner_sizedness {
            Sizedness::Sized => {
                quote! {
                    impl ::opaque_typedef::OpaqueTypedef for #ty_outer {
                        type Inner = #ty_inner;
                        type Error = ::opaque_typedef::Infallible;

                        unsafe fn from_inner_unchecked(inner: Self::Inner) -> Self {
                            Self { #name_inner: inner }
                        }
                        fn from_inner(inner: Self::Inner) -> Result<Self, Self::Error> {
                            Ok(Self { #name_inner: inner })
                        }
                        fn into_inner(self) -> Self::Inner {
                            self.#name_inner
                        }
                        fn as_inner(&self) -> &Self::Inner {
                            &self.#name_inner
                        }
                        unsafe fn as_inner_mut(&mut self) -> &mut Self::Inner {
                            &mut self.#name_inner
                        }
                    }
                }
            },
            Sizedness::Unsized => {
                quote! {
                    impl ::opaque_typedef::OpaqueTypedefUnsized for #ty_outer {
                        type Inner = #ty_inner;
                        type Error = ::opaque_typedef::Infallible;

                        unsafe fn from_inner_unchecked(inner: &Self::Inner) -> &Self {
                            ::std::mem::transmute(inner)
                        }
                        unsafe fn from_inner_unchecked_mut(inner: &mut Self::Inner) -> &mut Self {
                            ::std::mem::transmute(inner)
                        }
                        fn from_inner(inner: &Self::Inner) -> Result<&Self, Self::Error> {
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked(inner) })
                        }
                        fn from_inner_mut(inner: &mut Self::Inner) -> Result<&mut Self, Self::Error> {
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked_mut(inner) })
                        }
                        fn as_inner(&self) -> &Self::Inner {
                            &self.#name_inner
                        }
                        unsafe fn as_inner_mut(&mut self) -> &mut Self::Inner {
                            &mut self.#name_inner
                        }
                    }
                }
            },
        }
    }

    pub fn tokens_outer_expr_as_inner<T: ToTokens>(&self, expr: T) -> quote::Tokens {
        let ty_outer = self.ty_outer;
        match self.inner_sizedness {
            Sizedness::Sized => {
                quote!(<#ty_outer as ::opaque_typedef::OpaqueTypedef>::as_inner(#expr))
            },
            Sizedness::Unsized => {
                quote!(<#ty_outer as ::opaque_typedef::OpaqueTypedefUnsized>::as_inner(#expr))
            },
        }
    }
}
