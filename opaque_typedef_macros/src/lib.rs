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
use syn::DeriveInput;

use type_props::{Sizedness, TypeProps};

mod attrs;
mod derives;
mod type_props;
mod utils;


/// The entrypoint for a `#[derive(OpaqueTypedef)]`-ed type.
#[proc_macro_derive(OpaqueTypedef, attributes(opaque_typedef))]
pub fn opaque_typedef(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    let gen = gen_opaque_typedef_impls(input, Sizedness::Sized);
    gen.into()
}


/// The entrypoint for a `#[derive(OpaqueTypedefUnsized)]`-ed type.
#[proc_macro_derive(OpaqueTypedefUnsized, attributes(opaque_typedef))]
pub fn opaque_typedef_unsized(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    let gen = gen_opaque_typedef_impls(input, Sizedness::Unsized);
    gen.into()
}


/// Generates additional impls for a `#[derive(OpaqueTypedef*)]`-ed type.
fn gen_opaque_typedef_impls(input: DeriveInput, sizedness: Sizedness) -> quote::Tokens {
    let props = TypeProps::load(&input, sizedness);
    props.gen_impls()
}
