# Change Log

## [Unreleased]

### Changed (non-breaking)

* Resolve clippy lints (v0.0.206).
* Bump dependencies.
    + Now `opaque_typedef_macro` uses syn-0.14 and quote-0.6.


## [0.0.3] - 2018-04-28

Many operators are now supported!

### Added

* `std::ops::*` traits (for operators) support
  ([#7](https://github.com/lo48576/opaque_typedef/issues/7)).
    + The targets below are added:
        - Arithmetic unary: `Neg{,Ref}`
        - Arithmetic binary: `{Add,Div,Mul,Rem,Sub}{,Assign}{,Ref}{Self,Inner,InnerRev}`
        - Bitwise binary: `{BitAnd,BitOr,BitXor,Shl,Shr}{,Assign}{,Ref}{Self,Inner,InnerRev}`
        - Logical unary: `Not{,Ref}`


## [0.0.2] - 2018-04-10

* `opaque_typedef_macros` is [(almost) completely rewritten](https://github.com/lo48576/opaque_typedef/commit/5bf3c37aa1e89aff6812785b90f789e73e0f11af)!
* Many errors in README document are fixed.
* Custom comparator, validator, and generics are supported! :tada:

### Added

  * Add shortened notations for `As{Mut,Ref}*`, `Into*`: `As{Mut,Ref}(*)`, `Into(*)`
    ([#3](https://github.com/lo48576/opaque_typedef/issues/3)).
      + Note that `FooSelf` is shortened into `Foo(Self_)`, because `Self` is reserved keyword and
        cannot be used here.
  * Accept custom comparator function :tada:
    ([#4](https://github.com/lo48576/opaque_typedef/issues/4)).
      + Note that `opaque_typedef_macros` users are responsible to keep comparators behaviors consistent.
  * Add derive targets `Partial{Eq,Ord}Self` and `Ord`
    ([#4](https://github.com/lo48576/opaque_typedef/issues/4)).
      + These are `#[derive({PartialEq, PartialOrd, Ord})]` with custom comparators.
  * Accept validator function :tada:
    ([#5](https://github.com/lo48576/opaque_typedef/issues/5)).
  * Generics support :tada:
    ([#8](https://github.com/lo48576/opaque_typedef/issues/8)).

### Changed (breaking)

  * Add trait methods `OpaqueTypedef::try_from_inner` and `OpaqueTypedefUnsized::try_from_inner{,_mut}`
    ([dbd73af](https://github.com/lo48576/opaque_typedef/commit/dbd73afb7e24aa44c30d032dd88400f2e0766ddc#diff-ced119d73487961f323daca79cbdc0e5)).
  * Change return types of trait methods
    ([dbd73af](https://github.com/lo48576/opaque_typedef/commit/dbd73afb7e24aa44c30d032dd88400f2e0766ddc#diff-ced119d73487961f323daca79cbdc0e5)).
      + `OpaqueTypedef::from_inner` now returns `Self`, not `Result<Self, Self::Error>`.
      + `OpaqueTypedefUnsized::try_from_inner{,_mut}` now returns `&{,mut }Self`,
        not `Result<&{,mut }Self, Self::Error>`.
  * Add `std::fmt::Debug` trait bound to `OpaqueTypedef{,Unsized}::Error` type.
    ([dbd73af](https://github.com/lo48576/opaque_typedef/commit/dbd73afb7e24aa44c30d032dd88400f2e0766ddc#diff-ced119d73487961f323daca79cbdc0e5)).
      + Usual error types (typically implements `std::error::Error` implement `std::fmt::Debug` and
        `std::fmt::Display`, so you may not be affected in most cases.

### Changed (non-breaking)

  * Bump dependencies.
      + Use `quote ^0.5` and `syn ^0.13.1` crates.
        They had breaking changes (from `quote ^0.3` and `syn ^0.11`),
        and it is why `opaque_typedef_macros` is totally rewritten
        ([5bf3c37](https://github.com/lo48576/opaque_typedef/commit/5bf3c37aa1e89aff6812785b90f789e73e0f11af)).
      + Use `strum ^0.9`.

### Fixed (breaking)

  * Require opaque typedef of unsized types to be `#[repr(C)]` or `#[repr(transparent)]`
    ([#1](https://github.com/lo48576/opaque_typedef/issues/1)).
  * `#[opaque_typedef(deref(deref{,mut} = ..))]` now don't require
    `#[opaque_typedef(deref(target = ..))]`.
      + If omitted, the inner type is used as deref target.


## [0.0.1] - 2017-12-06

Initial release.

### Added

  * `OpaqueTypedef{,Unsized}` traits and `#[derive(OpaqueTypedef{,Unsized})]` custom derive.
  * Many derive targets.
    See <https://github.com/lo48576/opaque_typedef/blob/aafe5da5d0eb686cae0a5c5425ff91bbccaa8de4/README.md#automatic-derive-for-many-std-traits> for detail.



[Unreleased]: <https://github.com/lo48576/opaque_typedef/compare/v0.0.3...develop>
[0.0.3]: <https://github.com/lo48576/opaque_typedef/compare/v0.0.3>
[0.0.2]: <https://github.com/lo48576/opaque_typedef/releases/tag/v0.0.2>
[0.0.1]: <https://github.com/lo48576/opaque_typedef/releases/tag/v0.0.1>
