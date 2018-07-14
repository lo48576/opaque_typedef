//! Opaque typedef for `i32`.

/// Even `i32`.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(
    derive(
        Add(Self_),
        AddRef(Self_),
        AddAssign(Self_),
        AddAssignRef(Self_),
        Binary,
        Deref,
        Display,
        FromInner,
        PartialEq(Inner, InnerRev),
        PartialOrd(Inner, InnerRev),
        LowerHex,
        Octal,
        UpperHex
    )
)]
#[opaque_typedef(
    validation(
        validator = "validate_even32",
        error_type = "OddError",
        error_msg = "Failed to create `Even32`"
    )
)]
pub struct Even32(i32);

impl Even32 {
    /// Returns the inner `i32` even value.
    pub fn to_i32(self) -> i32 {
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
