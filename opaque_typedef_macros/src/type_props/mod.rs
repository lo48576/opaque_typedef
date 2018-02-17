//! Type properties.

use quote;
use syn;
use syn::DeriveInput;

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


/// Properties of a type with `#[derive(OpaqueTypedef*)]`.
#[derive(Clone)]
pub struct TypeProps<'a> {
    /// Outer type.
    pub ty_outer: &'a syn::Ident,
    /// Inner field.
    pub field_inner: Field<'a>,
    /// Sizedness of the inner type.
    pub inner_sizedness: Sizedness,
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
        unimplemented!()
    }
}
