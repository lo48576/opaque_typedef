# opaque\_typedef

[![Build Status](https://travis-ci.org/lo48576/opaque_typedef.svg?branch=develop)](https://travis-ci.org/lo48576/opaque_typedef)  
opaque_typedef:
[![Latest version](https://img.shields.io/crates/v/opaque_typedef.svg)](https://crates.io/crates/opaque_typedef)
[![Documentation](https://docs.rs/opaque_typedef/badge.svg)](https://docs.rs/opaque_typedef)  
opaque_typedef_macros:
[![Latest version](https://img.shields.io/crates/v/opaque_typedef_macros.svg)](https://crates.io/crates/opaque_typedef_macros)
[![Documentation](https://docs.rs/opaque_typedef_macros/badge.svg)](https://docs.rs/opaque_typedef_macros)

This is a proc-macro crate for [the Rust programming language](https://www.rust-lang.org/).

This crate helps developers to define opaque typedef (strong typedef) types easily with less boilerplates.

**NOTE**: This library is under development.

## Opaque typedef

You may want to define a new type, with the same internal representation as other types but without implicit type conversion.
Real world example:

  * [`UncasedStr` in rocket crate](https://docs.rs/rocket/0.3.3/rocket/http/uncased/struct.UncasedStr.html) (whose internal representation is `str`)
  * [`NotNaN` in ordered\_float crate](https://docs.rs/ordered-float/0.5.0/ordered_float/struct.NotNaN.html) (whose internal representation is floating point number types (usually `f32` and `f64`))

These types usually have additional restriction to internal type (`UncasedStr` has looser comparison function and `NotNan` cannot have NaN)
and you may want to implement some traits and don't want to implement some other traits
(for example, you may want `From<&str> for &UncasedStr` but don't want `Deref<Target=str> for UncasedStr`).

opaque\_typedef crate helps you "derive" specific traits (i.e. reuse the traits implemented for internal types) for your type.

To see example, see files under the `opaque_typedef_tests/src/` directory.

## Terms
Think `struct Outer(Inner);`:

  * **Inner type** means `Inner` type.
  * **Outer type** means `Outer` type.
  * `Outer` is **opaque typedef** (strong typedef) of `Inner`.
  * **Unsized type** means `str`, `[i32]`, or something (usually slice types).
  * **Sized type** means `String`, `i32`, `&u8`, or something.

## How to use

### 1. Specify "extern crate"

`Cargo.toml`:

```
[dependencies]
opaque_typedef = "^0.0.1"
opaque_typedef_macros = "^0.0.1"
```

`lib.rs` or `main.rs`:

```
extern crate opaque_typedef;
#[macro_use]
extern crate opaque_typedef_macros;
```

### 2. Derive `OpaqueTypedef` for sized types, `OpaqueTypedefUnsized` for unsized types

Sized type:

```
/// My owned string.
#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
pub struct MyString(String);
```

Unsized type:

```
/// My string slice.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
pub struct MyStr(str);
```

Then you can use `OpaqueTypedef` trait or `OpaqueTypedefUnsized` trait.
It will be useful to implement methods for your types!

### 3. Specify if the mutable reference can be used for deriving traits (optional)

If you want opaque\_typedef to derive traits who might return mutable reference to inner value (such as `DerefMut`, `AsMut`)
or traits who might mutate inner value (such as `AddAssign`), you should specify `#[opaque_typedef(allow_mut_ref)]`.


```
/// My string slice.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
#[opaque_typedef(allow_mut_ref)]
pub struct MyStr(str);
```

If you don't specify it, opaque\_typedef refuses "derive"s such as `#[opaque_typedef(derive(DerefMut))]`

### 4. "Derive" more traits

You can specify traits with `#[opaque_typedef(derive(Trait1, Trait2, ...))]`.

For example:

```
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
#[opaque_typedef(derive(AsciiExt, AsMutDeref, AsMutSelf, AsRefDeref, AsRefSelf, DefaultRef,
                        Deref, DerefMut, Display, FromInner, IntoArc, IntoBox, IntoRc,
                        IntoInner, PartialEq(Inner, InnerCow, SelfCow),
                        PartialOrd(Inner, InnerCow, SelfCow)))]
#[opaque_typedef(allow_mut_ref)]
pub struct MyStr(str);
```

Note that some traits can be shortened:

  * `PartialEqInner` can be written as `PartialEq(Inner)`
  * `PartialOrdInner, PartialOrdSelfCow` can be written as `PartialOrd(Inner, SelfCow)`

To see lists of "derive"-able items, read the rest of the document or see
[the source (`Derive` enum in `opaque_typedef_macros/src/derives.rs`)](https://github.com/lo48576/opaque_typedef/blob/develop/opaque_typedef_macros/src/derives.rs).

## Features

### Defining basic constructions and casts

#### For sized type

`#[derive(OpaqueTypedef)]` implements `opaque_typedef::OpaqueTypedef` trait, and it has some basic and useful methods.

See <https://docs.rs/opaque_typedef/*/opaque_typedef/trait.OpaqueTypedef.html> for detail.

#### For unsized type

`#[derive(OpaqueTypedefUnsized)]` implements `opaque_typedef::OpaqueTypedefUnsized` trait, and it has some basic and useful methods.
Especially `OpaqueTypedefUnsized::from_inner()` would be very useful.

See <https://docs.rs/opaque_typedef/*/opaque_typedef/trait.OpaqueTypedefUnsized.html> for detail.

### Automatic derive for many std traits

The traits below are supported:

  * `AsciiExt` implements `std::ascii::AsciiExt for Outer`.
  * `AsMutDeref` implements `AsMut<DerefTarget> for Outer`.
  * `AsMutInner` implements `AsMut<Inner> for Outer`.
  * `AsMutSelf` implements `AsMut<Outer> for Outer`.
  * `AsRefDeref` implements `AsRef<DerefTarget> for Outer`.
  * `AsRefInner` implements `AsRef<Inner> for Outer`.
  * `AsRefSelf` implements `AsRef<Self> for Outer`.
  * `Binary` implements `std::fmt::Binary for Outer`.
  * `DefaultRef` implements `Default for &Outer`.
  * `Deref` implements `std::ops::Deref for Outer`.
  * `DerefMut` implements `std::ops::DerefMut for Outer`.
  * `Display` implements `std::fmt::Display for Outer`.
  * `FromInner` implements `From<Inner> for Outer`.
  * `IntoArc` implements `From<Outer> for Arc<Outer>`.
  * `IntoBox` implements `From<Outer> for Box<Outer>`.
  * `IntoInner` implements `From<Outer> for Inner`.
  * `IntoRc` implements `From<Outer> for Rc<Outer>`.
  * `LowerExp` implements `std::fmt::LowerExp for Outer`.
  * `LowerHex` implements `std::fmt::LowerHex for Outer`.
  * `Octal` implements `std::fmt::Octal for Outer`.
  * `PartialEqInner` implements `PartialEq<Inner> for Outer` and similar ones.
  * `PartialEqInnerCow` implements `PartialEq<Cow<Inner>> for Outer` and similar ones.
  * `PartialEqSelfCow` implements `PartialEq<Cow<Outer>> for Outer` and similar ones.
  * `PartialEqSelfCowAndInner` implements `PartialEq<Cow<Outer>> for Inner` and similar ones.
  * `PartialOrdInner` implements `PartialOrd<Inner> for Outer` and similar ones.
  * `PartialOrdInnerCow` implements `PartialOrd<Cow<Inner>> for Outer` and similar ones.
  * `PartialOrdSelfCow` implements `PartialOrd<Cow<Outer>> for Outer` and similar ones.
  * `PartialOrdSelfCowAndInner` implements `PartialOrd<Cow<Outer>> for Inner` and similar ones.
  * `Pointer` implements `std::fmt::Pointer for Outer`.
  * `UpperExp` implements `std::fmt::UpperExp for Outer`.
  * `UpperHex` implements `std::fmt::UpperHex for Outer`.

Note that some (such as `DefaultRef`) are available only for sized types.

## TODO

  * Custom comparison functions
  * Validation on conversion from inner type into outer type
  * More traits
      + Especially `std::ops::*` binary operators
      + Nightly-only traits (`TryFrom`, `TryInto`, ...)
  * Type parameters support

## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE.txt](LICENSE-APACHE.txt) or https://www.apache.org/licenses/LICENSE-2.0 )
* MIT license ([LICENSE-MIT.txt](LICENSE-MIT.txt) or https://opensource.org/licenses/MIT )

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you,
as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
