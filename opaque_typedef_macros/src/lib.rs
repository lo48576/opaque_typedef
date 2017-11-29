//! Custom derives for easy opaque typedef.
#![recursion_limit = "128"]

extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate strum;
#[macro_use]
extern crate strum_macros;
extern crate syn;

use proc_macro::TokenStream;

use utils::{Sizedness, TypeProperties};

mod attrs;
mod derives;
mod fields;
mod names;
mod utils;


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
