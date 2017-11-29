//! Traits for `opaque_typedef_macros`.
#![warn(missing_docs)]


/// An error type that indicates the error should never happen.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Never {}


/// Common functions for opaque typedef-ed sized types.
pub trait OpaqueTypedef: Sized {
    /// Inner type.
    type Inner;
    /// Validation error type.
    type Error;

    /// Creates a new value from the inner value without validation.
    unsafe fn from_inner_unchecked(inner: Self::Inner) -> Self;
    /// Creates a new value from the inner value with validation.
    fn from_inner(inner: Self::Inner) -> Result<Self, Self::Error>;
    /// Takes and returns the inner value with its ownership.
    fn into_inner(self) -> Self::Inner;
    /// Returns the reference to the inner value.
    fn as_inner(&self) -> &Self::Inner;
    /// Returns the mutable reference to the inner value.
    unsafe fn as_inner_mut(&mut self) -> &mut Self::Inner;
}


/// Common functions for opaque typedef-ed unsized types.
pub trait OpaqueTypedefUnsized {
    /// Inner type.
    type Inner: ?Sized;
    /// Validation error type.
    type Error;

    /// Creates a reference from the inner reference without validation.
    unsafe fn from_inner_unchecked(inner: &Self::Inner) -> &Self;
    /// Creates a mutable reference from the inner mutable reference without validation.
    unsafe fn from_inner_unchecked_mut(inner: &mut Self::Inner) -> &mut Self;
    /// Creates a reference from the inner reference with validation.
    fn from_inner(inner: &Self::Inner) -> Result<&Self, Self::Error>;
    /// Creates a mutable reference from the inner mutable reference with validation.
    fn from_inner_mut(inner: &mut Self::Inner) -> Result<&mut Self, Self::Error>;
    /// Returns the inner reference.
    fn as_inner(&self) -> &Self::Inner;
    /// Returns the inner mutable reference.
    unsafe fn as_inner_mut(&mut self) -> &mut Self::Inner;
}
