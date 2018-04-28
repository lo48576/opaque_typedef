//! Opaque typedef for `i32`.


/// `i32`.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(
    derive(
        Add(Self_, Inner, InnerRev),
        AddRef(Self_, Inner, InnerRev),
        AddAssign(Self_, Inner),
        AddAssignRef(Self_, Inner),
        BitAnd(Self_, Inner, InnerRev),
        BitAndRef(Self_, Inner, InnerRev),
        BitAndAssign(Self_, Inner),
        BitAndAssignRef(Self_, Inner),
        BitOr(Self_, Inner, InnerRev),
        BitOrRef(Self_, Inner, InnerRev),
        BitOrAssign(Self_, Inner),
        BitOrAssignRef(Self_, Inner),
        BitXor(Self_, Inner, InnerRev),
        BitXorRef(Self_, Inner, InnerRev),
        BitXorAssign(Self_, Inner),
        BitXorAssignRef(Self_, Inner),
        Div(Self_, Inner, InnerRev),
        DivRef(Self_, Inner, InnerRev),
        DivAssign(Self_, Inner),
        DivAssignRef(Self_, Inner),
        Mul(Self_, Inner, InnerRev),
        MulRef(Self_, Inner, InnerRev),
        MulAssign(Self_, Inner),
        MulAssignRef(Self_, Inner),
        Neg,
        NegRef,
        Not,
        NotRef,
        Rem(Self_, Inner, InnerRev),
        RemRef(Self_, Inner, InnerRev),
        RemAssign(Self_, Inner),
        RemAssignRef(Self_, Inner),
        Shl(Self_, Inner, InnerRev),
        ShlRef(Self_, Inner, InnerRev),
        ShlAssign(Self_, Inner),
        ShlAssignRef(Self_, Inner),
        Shr(Self_, Inner, InnerRev),
        ShrRef(Self_, Inner, InnerRev),
        ShrAssign(Self_, Inner),
        ShrAssignRef(Self_, Inner),
        Sub(Self_, Inner, InnerRev),
        SubRef(Self_, Inner, InnerRev),
        SubAssign(Self_, Inner),
        SubAssignRef(Self_, Inner),
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
