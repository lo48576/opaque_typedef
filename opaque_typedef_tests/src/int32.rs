//! Opaque typedef for `i32`.

/// `i32`.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(
    derive(
        Add(Self, Inner, InnerRev),
        AddRef(Self, Inner, InnerRev),
        AddAssign(Self, Inner),
        AddAssignRef(Self, Inner),
        BitAnd(Self, Inner, InnerRev),
        BitAndRef(Self, Inner, InnerRev),
        BitAndAssign(Self, Inner),
        BitAndAssignRef(Self, Inner),
        BitOr(Self, Inner, InnerRev),
        BitOrRef(Self, Inner, InnerRev),
        BitOrAssign(Self, Inner),
        BitOrAssignRef(Self, Inner),
        BitXor(Self, Inner, InnerRev),
        BitXorRef(Self, Inner, InnerRev),
        BitXorAssign(Self, Inner),
        BitXorAssignRef(Self, Inner),
        Div(Self, Inner, InnerRev),
        DivRef(Self, Inner, InnerRev),
        DivAssign(Self, Inner),
        DivAssignRef(Self, Inner),
        Mul(Self, Inner, InnerRev),
        MulRef(Self, Inner, InnerRev),
        MulAssign(Self, Inner),
        MulAssignRef(Self, Inner),
        Neg,
        NegRef,
        Not,
        NotRef,
        Rem(Self, Inner, InnerRev),
        RemRef(Self, Inner, InnerRev),
        RemAssign(Self, Inner),
        RemAssignRef(Self, Inner),
        Shl(Self, Inner, InnerRev),
        ShlRef(Self, Inner, InnerRev),
        ShlAssign(Self, Inner),
        ShlAssignRef(Self, Inner),
        Shr(Self, Inner, InnerRev),
        ShrRef(Self, Inner, InnerRev),
        ShrAssign(Self, Inner),
        ShrAssignRef(Self, Inner),
        Sub(Self, Inner, InnerRev),
        SubRef(Self, Inner, InnerRev),
        SubAssign(Self, Inner),
        SubAssignRef(Self, Inner),
        AsMut(Deref, Self),
        AsRef(Deref, Self),
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
    pub fn to_i32(self) -> i32 {
        self.0
    }
}
