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


#[derive(Default, Clone)]
pub struct DerefSpec {
    /// Deref target type.
    pub ty_deref_target: Option<syn::Type>,
    /// Converter function from inner type to target type.
    ///
    /// The function should have `&Inner -> &Target` type.
    pub fn_name_deref: Option<syn::Expr>,
    /// Converter function from inner type to target type.
    ///
    /// The function should have `&Inner -> &Target` type.
    pub fn_name_deref_mut: Option<syn::Expr>,
}


#[derive(Default, Clone)]
pub struct ValidationSpec {
    /// Validator.
    pub fn_validator: Option<syn::Expr>,
    /// Validation error type.
    pub ty_error: Option<syn::Type>,
    /// Validation error message.
    pub error_msg: Option<String>,
}

impl ValidationSpec {
    pub fn tokens_try_validated<T: quote::ToTokens>(&self, inner: T) -> quote::Tokens {
        match self.fn_validator {
            Some(ref validator) => quote!(#validator(#inner)?),
            None => inner.into_tokens(),
        }
    }

    pub fn tokens_validated<T: quote::ToTokens>(&self, inner: T) -> quote::Tokens {
        let validation_result = match self.fn_validator {
            Some(ref validator) => quote!(#validator(#inner)),
            None => return inner.into_tokens(),
        };
        match self.error_msg {
            Some(ref msg) => quote!(#validation_result.expect(#msg)),
            None => quote!(#validation_result.unwrap()),
        }
    }

    pub fn tokens_ty_error(&self) -> quote::Tokens {
        match self.ty_error {
            Some(ref ty) => ty.into_tokens(),
            None => quote!(::opaque_typedef::Infallible),
        }
    }
}


/// Properties of a type with `#[derive(OpaqueTypedef*)]`.
#[derive(Clone)]
pub struct TypeProps<'a> {
    /// Outer type.
    pub ty_outer: &'a syn::Ident,
    /// Inner field.
    pub field_inner: Field<'a>,
    /// Generics.
    pub generics: &'a syn::Generics,
    /// Impl generics (cache).
    pub impl_generics: syn::ImplGenerics<'a>,
    /// Type generics (cache).
    pub type_generics: syn::TypeGenerics<'a>,
    /// Where clause (cache).
    pub where_clause: Option<&'a syn::WhereClause>,
    /// Sizedness of the inner type.
    pub inner_sizedness: Sizedness,
    /// Derive target traits.
    pub derives: Vec<Derive>,
    /// Deref spec.
    pub deref_spec: DerefSpec,
    /// Whether the mutable reference to the inner field is allowed.
    pub is_mut_ref_allowed: bool,
    /// Validation spec.
    pub validation_spec: ValidationSpec,
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
        let impl_generics = &self.impl_generics;
        let type_generics = &self.type_generics;
        let where_clause = &self.where_clause;
        let ty_error = self.validation_spec.tokens_ty_error();
        let inner_try_validated = self.validation_spec.tokens_try_validated(quote!(__inner));
        let inner_validated = self.validation_spec.tokens_validated(quote!(__inner));
        match self.inner_sizedness {
            Sizedness::Sized => {
                quote! {
                    impl #impl_generics ::opaque_typedef::OpaqueTypedef for #ty_outer #type_generics
                    #where_clause
                    {
                        type Inner = #ty_inner;
                        type Error = #ty_error;

                        unsafe fn from_inner_unchecked(__inner: Self::Inner) -> Self {
                            Self { #name_inner: __inner }
                        }
                        fn try_from_inner(__inner: Self::Inner) -> Result<Self, Self::Error> {
                            Ok(Self { #name_inner: #inner_try_validated })
                        }
                        fn from_inner(__inner: Self::Inner) -> Self {
                            Self { #name_inner: #inner_validated }
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
                    impl #impl_generics
                        ::opaque_typedef::OpaqueTypedefUnsized for #ty_outer #type_generics
                    #where_clause
                    {
                        type Inner = #ty_inner;
                        type Error = #ty_error;

                        unsafe fn from_inner_unchecked(__inner: &Self::Inner) -> &Self {
                            ::std::mem::transmute(__inner)
                        }
                        unsafe fn from_inner_unchecked_mut(__inner: &mut Self::Inner) -> &mut Self {
                            ::std::mem::transmute(__inner)
                        }
                        fn try_from_inner(__inner: &Self::Inner) -> Result<&Self, Self::Error> {
                            let __inner = #inner_try_validated;
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked(__inner) })
                        }
                        fn from_inner(__inner: &Self::Inner) -> &Self {
                            let __inner = #inner_validated;
                            unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked(__inner) }
                        }
                        fn try_from_inner_mut(__inner: &mut Self::Inner) -> Result<&mut Self, Self::Error> {
                            let __inner = #inner_try_validated;
                            Ok(unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked_mut(__inner) })
                        }
                        fn from_inner_mut(__inner: &mut Self::Inner) -> &mut Self {
                            let __inner = #inner_validated;
                            unsafe { <Self as ::opaque_typedef::OpaqueTypedefUnsized>::from_inner_unchecked_mut(__inner) }
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
        let type_generics = &self.type_generics;
        let helper_trait = self.helper_trait();
        quote!(<#ty_outer #type_generics as #helper_trait>::as_inner(#expr))
    }

    pub fn tokens_outer_expr_as_inner_mut<T: ToTokens>(&self, expr: T) -> quote::Tokens {
        // The caller is responsible to ensure `allow_mut_ref` is specified.
        assert!(
            self.is_mut_ref_allowed,
            "opaque_typedef internal error: Caller should ensure `allow_mut_ref` is specified"
        );

        let ty_outer = self.ty_outer;
        let type_generics = &self.type_generics;
        let helper_trait = self.helper_trait();
        quote! {
            unsafe {
                <#ty_outer #type_generics as #helper_trait>::as_inner_mut(#expr)
            }
        }
    }

    pub fn tokens_ty_deref_target(&self) -> quote::Tokens {
        match self.deref_spec.ty_deref_target {
            Some(ref ty) => ty.into_tokens(),
            None => (&self.field_inner.ty()).into_tokens(),
        }
    }

    pub fn tokens_fn_deref(&self) -> quote::Tokens {
        self.deref_spec
            .fn_name_deref
            .as_ref()
            .map_or_else(|| quote!(), |name| name.into_tokens())
    }

    pub fn tokens_fn_deref_mut(&self) -> quote::Tokens {
        self.deref_spec
            .fn_name_deref_mut
            .as_ref()
            .map_or_else(|| quote!(), |name| name.into_tokens())
    }
}
