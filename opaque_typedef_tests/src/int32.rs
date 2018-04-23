//! Opaque typedef for `i32`.


/// `i32`.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(
    derive(
        Add(Self_, Inner, InnerRev),
        AddRef(Self_, Inner, InnerRev),
        AddAssign(Self_, Inner),
        AddAssignRef(Self_, Inner),
        AsMut(Deref, Self_),
        AsRef(Deref, Self_),
        Binary,
        Display,
        FromInner,
        PartialEq(Inner, InnerRev),
        PartialOrd(Inner, InnerRev),
        LowerHex,
        Octal,
        UpperHex
    )
)]
#[opaque_typedef(allow_mut_ref)]
pub struct Int32(i32);

impl Int32 {
    /// Returns the inner `i32` value.
    pub fn to_i32(&self) -> i32 {
        self.0
    }
}
