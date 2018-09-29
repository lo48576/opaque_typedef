# opaque\_typedef

[![Build Status](https://travis-ci.org/lo48576/opaque_typedef.svg?branch=develop)](https://travis-ci.org/lo48576/opaque_typedef)  
opaque_typedef:
[![Latest version](https://img.shields.io/crates/v/opaque_typedef.svg)](https://crates.io/crates/opaque_typedef)
[![Documentation](https://docs.rs/opaque_typedef/badge.svg)](https://docs.rs/opaque_typedef)  
opaque_typedef_macros:
[![Latest version](https://img.shields.io/crates/v/opaque_typedef_macros.svg)](https://crates.io/crates/opaque_typedef_macros)
<!--[![Documentation](https://docs.rs/opaque_typedef_macros/badge.svg)](https://docs.rs/opaque_typedef_macros)-->

This is a proc-macro crate for [the Rust programming language](https://www.rust-lang.org/).

This crate helps developers to define opaque typedef (strong typedef) types easily with less boilerplates.

**NOTE**: This library is under development and unstable.

## Opaque typedef

You may want to define a new type, with the same internal representation as other types but without implicit type conversion.
Real world example:

  * [`UncasedStr` in rocket crate](https://docs.rs/rocket/0.3.3/rocket/http/uncased/struct.UncasedStr.html) (whose internal representation is `str`)
  * [`NotNaN` in ordered\_float crate](https://docs.rs/ordered-float/0.5.0/ordered_float/struct.NotNaN.html) (whose internal representation is floating point number types (usually `f32` and `f64`))

These types usually have additional restriction to internal type (`UncasedStr` has looser comparison function and `NotNan` cannot have NaN)
and you may want to implement some traits and don't want to implement some other traits
(for example, you may want `From<&str> for &UncasedStr` but don't want `Deref<Target=str> for UncasedStr`).

opaque\_typedef crate helps you "derive" specific traits (i.e. reuse the traits implemented for internal types) for your type.

To see example, see files under the [`opaque_typedef_tests/src/`](https://github.com/lo48576/opaque_typedef/tree/develop/opaque_typedef_tests/src) directory.

## Terms
Think `struct Outer(Inner);`:

  * **Inner type** means `Inner` type.
  * **Outer type** means `Outer` type.
  * `Outer` is **opaque typedef** (strong typedef) of `Inner`.
  * **Unsized type** means `str`, `[i32]`, or something (usually slice types).
  * **Sized type** means `String`, `i32`, `&u8`, or something.

## How to use

Examples are in [`opaque_typedef_tests/src/`](https://github.com/lo48576/opaque_typedef/tree/develop/opaque_typedef_tests/src).

### 1. Specify "extern crate"

`Cargo.toml`:

```toml
[dependencies]
opaque_typedef = "^0.0.4"
opaque_typedef_macros = "^0.0.4"
```

`lib.rs` or `main.rs`:

```rust
extern crate opaque_typedef;
#[macro_use]
extern crate opaque_typedef_macros;
```

### 2. Derive `OpaqueTypedef` for sized types, `OpaqueTypedefUnsized` for unsized types

Sized type:

```rust
/// My owned string.
#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
pub struct MyString(String);
```

Unsized type:

```rust
/// My string slice.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
#[repr(C)]
pub struct MyStr(str);
```

Note that `#[repr(C)]` (or `#[repr(transparent)]`) is necessary for unsized types.

Then you can use `OpaqueTypedef` trait or `OpaqueTypedefUnsized` trait.
It will be useful to implement methods for your types!

About the necessity of `#[repr(*)]`, see <https://github.com/lo48576/opaque_typedef/issues/1>.

### 3. Specify if the mutable reference can be used for deriving traits (optional)

If you want opaque\_typedef to derive traits who might return mutable reference to inner value (such as `DerefMut`, `AsMut`)
or traits who might mutate inner value (such as `AddAssign`), you should specify `#[opaque_typedef(allow_mut_ref)]`.


```rust
/// My string slice.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
#[repr(C)]
#[opaque_typedef(allow_mut_ref)]
pub struct MyStr(str);
```

If you don't specify it, opaque\_typedef refuses "derive"s such as `#[opaque_typedef(derive(DerefMut))]`

### 4. "Derive" more traits

You can specify traits with `#[opaque_typedef(derive(Trait1, Trait2, ...))]`.

For example:

```rust
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
#[repr(C)]
#[opaque_typedef(derive(AsciiExt, AsMut(Deref, Self), AsRef(Deref, Self), DefaultRef, Deref,
                        DerefMut, Display, FromInner, Into(Arc, Box, Rc, Inner),
                        PartialEq(Inner, InnerRev, InnerCow, InnerCowRev, SelfCow, SelfCowRev),
                        PartialOrd(Inner, InnerRev, InnerCow, InnerCowRev, SelfCow, SelfCowRev)))]
#[opaque_typedef(allow_mut_ref)]
pub struct MyStr(str);
```

Note that some traits can be shortened.
Examples:

  * `AsMutDeref` can be written as `AsMut(Deref)`
  * `AsRefSelf` can be written as `AsRef(Self)`
  * `IntoRc` can be written as `Into(Rc)`
  * `PartialEqInner` can be written as `PartialEq(Inner)`
  * `PartialOrdInner, PartialOrdSelfCow` can be written as `PartialOrd(Inner, SelfCow)`

To see lists of "derive"-able items, read the rest of the document or see
[the source (`Derive` enum in `opaque_typedef_macros/src/derives/mod.rs`)](https://github.com/lo48576/opaque_typedef/blob/develop/opaque_typedef_macros/src/derives/mod.rs).

To see full list of shortened notations for "derive"-able items, see
[`Derive::append_from_nested_names` method at `opaque_typedef_macros/src/derives/mod.rs`)](https://github.com/lo48576/opaque_typedef/blob/develop/opaque_typedef_macros/src/derives/mod.rs).

### 4.1. Specify deref target (optional)

If you specify `Deref`, `DerefMut`, `AsRefDeref` or something related to `Deref`, you can also specify "deref target" by `#[opaque_typedef(deref(...))]`.

```rust
/// My owned string.
#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(derive(AsMut(Deref, Inner), AsRef(Deref, Inner), Deref, DerefMut, Display,
                        FromInner, IntoInner, PartialEq(Inner, InnerRev),
                        PartialOrd(Inner, InnerRev)))]
#[opaque_typedef(deref(target = "str", deref = "String::as_str",
                       deref_mut = "String::as_mut_str"))]
#[opaque_typedef(allow_mut_ref)]
pub struct MyString {
    inner: String,
}
```

Opaque\_typedef uses the inner type as the default deref target type, but you can use a different type as the example above.

  * `target`:
      + Deref target type.
  * `deref`:
      + Conversion function from a reference to the **inner** type into a **reference** to the outer type.
      + The function should implement `Fn(&Inner) -> &DerefTarget`.
  * `deref_mut`:
      + Conversion function from a mutable reference to the **inner** type into a mutable **reference** to the outer type.
      + The function should implement `Fn(&mut Inner) -> &mut DerefTarget`.

In the example, `AsMutInner` implements `AsMut<String> for MyString`, and `AsMutDeref` implements `AsMut<str> for MyString`.

If you don't specify `#[opaque_typedef(allow_mut_ref)]`, `deref_mut` would not be used and you can omit it.

### 5. Specify custom validator (optional)

You can specify custom validator.
The value of inner type is validated on conversion into outer type.
By custom validator, you can restrict the inner value.

To use custom validator, specify these attributes:

  * `validator`
      + Validator function.
        This should have types such as `Inner -> Result<Inner, Error>`.
          - For sized types, `Inner -> Result<Inner, Error>`.
            Validator can modify the given value and return the modified value.
          - For unsized types, `&Inner -> Result<&Inner, Error>`.
  * `error_type`
      + Validation error type.
        Validator specified by `validator` should use this type as error.
      + This cannot be generic, and cannot use any type parameters of the outer types.
  * `error_msg` (optional)
      + Error message on panic when validation failed.
        This value is used when panickable conversion failed, for example,
        when invalid value is passed to `Outer::from_inner` (not `Outer::try_from_inner`).
      + Internally, `unwrap()` will be used to panic when `error_msg` is absent,
        and `expect(error_msg)` will be used when `error_msg` is specified.

The example below is taken from [`opaque_typedef_tests/src/even32.rs`](opaque_typedef_tests/src/even32.rs)
and [`opaque_typedef_tests/tests/even32.rs`](opaque_typedef_tests/tests/even32.rs).

```rust
/// Even `i32`.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(derive(Binary, Deref, Display, FromInner, PartialEq(Inner, InnerRev),
                        PartialOrd(Inner, InnerRev), LowerHex, Octal, UpperHex))]
#[opaque_typedef(validation(validator = "validate_even32", error_type = "OddError",
                            error_msg = "Failed to create `Even32`"))]
pub struct Even32(i32);

impl Even32 {
    /// Returns the inner `i32` even value.
    pub fn to_i32(&self) -> i32 {
        self.0
    }
}

/// A type of an error indicating the integer is an odd number, not even.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OddError;

fn validate_even32(v: i32) -> Result<i32, OddError> {
    if v % 2 == 0 {
        Ok(v)
    } else {
        Err(OddError)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn ok() {
        let v = Even32::from(42);
        assert_eq!(v.to_i32(), 42);
    }

    #[test]
    #[should_panic]
    fn from_odd() {
        // Panics with message "Failed to create `Even32`: OddError".
        let _ = Even32::from(3);
    }
}
```

### 6. Specify custom comparator (optional)

You can use custom implementations for `PartialEq` and `PartialOrd`.

To use custom comparator, specify these attributes:

  * `partial_eq`
      + Partial equality function.
        This should have types such as `&Inner -> &Inner -> bool`.
  * `partial_ord`
      + Partial order function.
        This should have types such as `&Inner -> &Inner -> Option<::std::cmp::Ordering>`.
  * `ord`
      + Total order function.
        This should have types such as `&Inner -> &Inner -> ::std::cmp::Ordering`.
      + `#[derive(Ord)]` doesn't use `PartialOrd::partial_cmp` impl, so they can be inconsistent by mistake.
        Remember to **keep them (including `PartialEq::eq`) consistent**.

The example below is taken from
[`opaque_typedef_tests/src/reverse_order.rs`](opaque_typedef_tests/src/reverse_order.rs)
and [`opaque_typedef_tests/tests/reverse_order.rs`](opaque_typedef_tests/tests/reverse_order.rs).

```rust
/// A wrapper type with reverse order.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(derive(AsciiExt, AsMut(Deref), AsRef(Deref), Binary, Deref, DerefMut, Display,
                        FromInner, LowerHex, Octal, PartialOrdSelf, UpperHex))]
#[opaque_typedef(cmp(partial_ord = "(|a, b| PartialOrd::partial_cmp(a, b).map(|o| o.reverse()))",
                     ord = "(|a, b| Ord::cmp(a, b).reverse())"))]
#[opaque_typedef(allow_mut_ref)]
pub struct ReverseOrderSized<T>(pub T);


#[test]
fn reverse_i32() {
    use std::cmp::Ordering;
    assert_eq!(ReverseOrderSized(3i32).partial_cmp(&ReverseOrderSized(2i32)), Some(Ordering::Less));
    assert!(ReverseOrderSized(3i32) < ReverseOrderSized(2i32));
    assert_eq!(ReverseOrderSized(3i32).cmp(&ReverseOrderSized(2i32)), Ordering::Less);
    assert_eq!(ReverseOrderSized(3i32).cmp(&ReverseOrderSized(3i32)), Ordering::Equal);
    assert_eq!(ReverseOrderSized(3i32).cmp(&ReverseOrderSized(4i32)), Ordering::Greater);
}
```

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

The traits below are supported.

Note that some (such as `DefaultRef`) are available only for sized types.

#### `std::convert`, type conversion

  * `As{Mut,Ref}{Deref,Inner,Self}`
      + `AsMutDeref` implements `AsMut<DerefTarget> for Outer`.
      + `AsMutInner` implements `AsMut<Inner> for Outer`.
      + `AsMutSelf` implements `AsMut<Outer> for Outer`.
      + `AsRefDeref` implements `AsRef<DerefTarget> for Outer`.
      + `AsRefInner` implements `AsRef<Inner> for Outer`.
      + `AsRefSelf` implements `AsRef<Self> for Outer`.
  * `Deref`, `DerefMut`
      + `Deref` implements `std::ops::Deref for Outer`.
      + `DerefMut` implements `std::ops::DerefMut for Outer`.
  * `Into{Arc,Box,Inner,Rc}`, `FromInner`
      + `IntoArc` implements `From<Outer> for Arc<Outer>` (if possible) or `Into<Arc<Outer>> for Outer`.
      + `IntoBox` implements `From<Outer> for Box<Outer>` (if possible) or `Into<Box<Outer>> for Outer`.
      + `IntoInner` implements `From<Outer> for Inner`.
      + `IntoRc` implements `From<Outer> for Rc<Outer>` (if possible) or `Into<Rc<Outer>> for Outer`.
      + `FromInner` implements `From<Inner> for Outer`.

#### `std::fmt`

  * `std::fmt::*`
      + `Binary` implements `std::fmt::Binary for Outer`.
      + `Display` implements `std::fmt::Display for Outer`.
      + `LowerExp` implements `std::fmt::LowerExp for Outer`.
      + `LowerHex` implements `std::fmt::LowerHex for Outer`.
      + `Octal` implements `std::fmt::Octal for Outer`.
      + `Pointer` implements `std::fmt::Pointer for Outer`.
      + `UpperExp` implements `std::fmt::UpperExp for Outer`.
      + `UpperHex` implements `std::fmt::UpperHex for Outer`.

#### `std::cmp`

  * `Partial{Eq,Ord}{Inner,InnerCow,SelfCow}{,Rev}`
      + `PartialEqInner` implements `PartialEq<Inner> for Outer` and similar ones.
      + `PartialEqInnerRev` implements `PartialEq<Outer> for Inner` and similar ones.
          - This is reverse (operands order swapped) version of `PartialEqInner`.
      + `PartialEqInnerCow` implements `PartialEq<Cow<Inner>> for Outer` and similar ones.
      + `PartialEqInnerCowRev` implements `PartialEq<Outer> for Cow<Inner>` and similar ones.
          - This is reverse (operands order swapped) version of `PartialEqInnerCow`.
      + `PartialEqSelf` implements `PartialEq<Outer> for Outer` and similar ones.
          - This is very similar to `#[derive(PartialEq)]`, but it will be useful with custom comparison.
      + `PartialEqSelfCow` implements `PartialEq<Cow<Outer>> for Outer` and similar ones.
      + `PartialEqSelfCowRev` implements `PartialEq<Outer> for Cow<Outer>` and similar ones.
          - This is reverse (operands order swapped) version of `PartialEqSelfCow`.
      + `PartialEqSelfCowAndInner` implements `PartialEq<Cow<Outer>> for Inner` and similar ones.
      + `PartialEqSelfCowAndInnerCow` implements `PartialEq<Inner> for Cow<Outer>` and similar ones.
          - This is reverse (operands order swapped) version of `PartialEqSelfCowAndInner`.
      + `PartialOrdInner` implements `PartialOrd<Inner> for Outer` and similar ones.
      + `PartialOrdInnerRev` implements `PartialOrd<Outer> for Inner` and similar ones.
          - This is reverse (operands order swapped) version of `PartialOrdInner`.
      + `PartialOrdInnerCow` implements `PartialOrd<Cow<Inner>> for Outer` and similar ones.
      + `PartialOrdInnerCowRev` implements `PartialOrd<Outer> for Cow<Inner>` and similar ones.
          - This is reverse (operands order swapped) version of `PartialOrdInnerCow`.
      + `PartialOrdSelf` implements `PartialOrd<Outer> for Outer` and similar ones.
          - This is very similar to `#[derive(PartialOrd)]`, but it will be useful with custom comparison.
      + `PartialOrdSelfCow` implements `PartialOrd<Cow<Outer>> for Outer` and similar ones.
      + `PartialOrdSelfCowRev` implements `PartialOrd<Outer> for Cow<Outer>` and similar ones.
          - This is reverse (operands order swapped) version of `PartialOrdSelfCow`.
      + `PartialOrdSelfCowAndInner` implements `PartialOrd<Cow<Outer>> for Inner` and similar ones.
      + `PartialOrdSelfCowAndInnerCow` implements `PartialOrd<Inner> for Cow<Outer>` and similar ones.
          - This is reverse (operands order swapped) version of `PartialOrdSelfCowAndInner`.
  * `Ord`
      + `Ord` implements `std::cmp::Ord for Outer`.
          - This is very similar to `#[derive(Ord)]`, but it will be useful with custom comparison.

#### `std::ops`

  * Unary ops
      + `Neg{,Ref}`
      + `Not{,Ref}`
  * Binary ops
      + `{Add,BitAnd,BitOr,BitXor,Div,Mul,Rem,Shl,Shr,Sub}{,Assign}{,Ref}{Self,Inner,InnerRev}`


#### Others

  * `AsciiExt` implements `std::ascii::AsciiExt for Outer`.
  * `DefaultRef` implements `Default for &Outer`.


## TODO

  * More traits
      + Nightly-only traits (`TryFrom`, `TryInto`, ...) ([#6](https://github.com/lo48576/opaque_typedef/issues/6))
  * Support types with multiple fields ([#9](https://github.com/lo48576/opaque_typedef/issues/9))

## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE.txt](LICENSE-APACHE.txt) or https://www.apache.org/licenses/LICENSE-2.0 )
* MIT license ([LICENSE-MIT.txt](LICENSE-MIT.txt) or https://opensource.org/licenses/MIT )

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you,
as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
