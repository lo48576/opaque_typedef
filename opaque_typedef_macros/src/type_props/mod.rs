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

                        unsafe fn from_inner_unchecked(__inner: Self::Inner) -> Self {
                            Self { #name_inner: __inner }
                        }
                        fn from_inner(__inner: Self::Inner) -> Result<Self, Self::Error> {
                            Ok(Self { #name_inner: __inner })
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

                        unsafe fn from_inner_unchecked(__inner: &Self::Inner) -> &Self {
                            ::std::mem::transmute(__inner)
                        }
                        unsafe fn from_inner_unchecked_mut(__inner: &mut Self::Inner) -> &mut Self {
                            ::std::mem::transmute(__inner)
                        }
                        fn from_inner(__inner: &Self::Inner) -> Result<&Self, Self::Error> {
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked(__inner) })
                        }
                        fn from_inner_mut(__inner: &mut Self::Inner) -> Result<&mut Self, Self::Error> {
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked_mut(__inner) })
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

    /// Returns helper trait path.
    pub fn helper_trait(&self) -> quote::Tokens {
        match self.inner_sizedness {
            Sizedness::Sized => quote!(::opaque_typedef::OpaqueTypedef),
            Sizedness::Unsized => quote!(::opaque_typedef::OpaqueTypedefUnsized),
        }
    }

    pub fn tokens_outer_expr_as_inner<T: ToTokens>(&self, expr: T) -> quote::Tokens {
        let ty_outer = self.ty_outer;
        let helper_trait = self.helper_trait();
        quote!(<#ty_outer as #helper_trait>::as_inner(#expr))
    }

    pub fn tokens_outer_expr_as_inner_mut<T: ToTokens>(&self, expr: T) -> quote::Tokens {
        // FIXME: Ensure `#[opaque_typedef(allow_mut_ref)]` is specified.
        let ty_outer = self.ty_outer;
        let helper_trait = self.helper_trait();
        quote! {
            unsafe {
                <#ty_outer as #helper_trait>::as_inner_mut(#expr)
            }
        }
    }

    pub fn tokens_fn_deref_and_ty_deref_target(&self) -> (quote::Tokens, quote::Tokens) {
        match self.deref_spec {
            Some(ref spec) => (
                (&spec.fn_name_deref).into_tokens(),
                (&spec.ty_deref_target).into_tokens(),
            ),
            None => (quote!(), (&self.field_inner.ty()).into_tokens()),
        }
    }

    pub fn tokens_fn_deref_mut_and_ty_deref_target(&self) -> (quote::Tokens, quote::Tokens) {
        match self.deref_spec {
            Some(ref spec) => {
                let ty_deref_target = (&spec.ty_deref_target).into_tokens();
                let fn_name_deref_mut = match spec.fn_name_deref_mut {
                    Some(ref name) => name.into_tokens(),
                    None => quote!(),
                };
                (fn_name_deref_mut, ty_deref_target)
            },
            None => (quote!(), (&self.field_inner.ty()).into_tokens()),
        }
    }
}
