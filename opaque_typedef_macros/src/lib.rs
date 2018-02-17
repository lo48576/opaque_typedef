//! Custom derives for easy opaque typedef.

extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use syn::DeriveInput;


/// Sizedness of a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Sizedness {
    /// Sized.
    Sized,
    /// Unsized.
    Unsized,
}


/// The entrypoint for a `#[derive(OpaqueTypedef)]`-ed type.
#[proc_macro_derive(OpaqueTypedef)]
pub fn opaque_typedef(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    let gen = gen_opaque_typedef_impls(input, Sizedness::Sized);
    gen.into()
}


/// The entrypoint for a `#[derive(OpaqueTypedefUnsized)]`-ed type.
#[proc_macro_derive(OpaqueTypedefUnsized)]
pub fn opaque_typedef_unsized(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    let gen = gen_opaque_typedef_impls(input, Sizedness::Unsized);
    gen.into()
}


/// Generates additional impls for a `#[derive(OpaqueTypedef*)]`-ed type.
fn gen_opaque_typedef_impls(input: DeriveInput, sizedness: Sizedness) -> quote::Tokens {
    let _ = input;
    let _ = sizedness;
    unimplemented!()
}
