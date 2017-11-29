//! Custom derives for easy opaque typedef.

extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;

mod attrs;
mod fields;
mod names;


/// The entrypoint for `#[derive(OpaqueTypedef)]`.
#[proc_macro_derive(OpaqueTypedef, attributes(opaque_typedef))]
pub fn opaque_typedef(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = gen_opaque_typedef(&ast, Sizedness::Sized);
    gen.parse().unwrap()
}


/// The entrypoint for `#[derive(OpaqueTypedefUnsized)]`.
#[proc_macro_derive(OpaqueTypedefUnsized, attributes(opaque_typedef))]
pub fn opaque_typedef_slice(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = gen_opaque_typedef(&ast, Sizedness::Unsized);
    gen.parse().unwrap()
}


/// Generates additional implementations for a `#[derive(OpaqueTypedef*)]`-ed type.
fn gen_opaque_typedef(ast: &syn::DeriveInput, inner_sizedness: Sizedness) -> quote::Tokens {
    let props = TypeProperties::from_ast(ast, inner_sizedness);
    props.impl_traits()
}


/// Sizedness of the inner type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Sizedness {
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
struct TypeProperties<'a> {
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
        let _attrs = attrs::get_metaitems(&ast.attrs, names::ATTR_NAME);
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
        quote!{}
    }
}
