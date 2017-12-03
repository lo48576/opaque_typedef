//! Opaque typedef for `i32`.


/// Even `i32`.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(derive(Binary, Deref, Display, FromInner, PartialEqInner))]
pub struct Even32(i32);

impl Even32 {
    /// Creates a new `Even32` from the given `i32` value.
    ///
    /// Panics when the given value is odd.
    pub fn from_i32(v: i32) -> Self {
        assert_eq!(v % 2, 0);
        Even32(v)
    }

    /// Returns the inner `i32` even value.
    pub fn to_i32(&self) -> i32 {
        self.0
    }
}
