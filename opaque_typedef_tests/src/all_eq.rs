//! All same.


/// All same.
#[derive(Default, Debug, Clone, Copy, Eq, OpaqueTypedef)]
// `IntoInner` cannot be used (and even you can't implement it manually),
// because it implements
// `impl<T> Into<T> for ReverseOrderSized<T>`,
// and downstream can immplement `pub struct MyType;` and
// `impl<T> From<ReverseOrderSized<T>> for MyType`,
// then they conflicts (in case of `T = MyType`, they are both
// `From<ReverseOrderSized<MyType>> for MyType`).
#[opaque_typedef(
    derive(
        AsciiExt,
        AsMut(Deref),
        AsRef(Deref),
        Binary,
        Deref,
        DerefMut,
        Display,
        FromInner,
        LowerHex,
        Octal,
        PartialEqSelf,
        UpperHex
    )
)]
#[opaque_typedef(cmp(partial_eq = "(|_, _| true)"))]
#[opaque_typedef(allow_mut_ref)]
pub struct AllEq<T>(pub T);
