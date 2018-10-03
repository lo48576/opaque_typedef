//! `opaque_typedef` code generator.
#![warn(missing_docs)]
#![recursion_limit = "128"]

extern crate proc_macro2;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

pub mod builder;
