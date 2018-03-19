//! A wrapper type with reverse order.


/// A wrapper type with reverse order.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Ord, Hash, OpaqueTypedef)]
// `IntoInner` cannot be used (and even you can't implement it manually),
// because it implements
// `impl<T> Into<T> for ReverseOrderSized<T>`,
// and downstream can immplement `pub struct MyType;` and
// `impl<T> From<ReverseOrderSized<T>> for MyType`,
// then they conflicts (in case of `T = MyType`, they are both
// `From<ReverseOrderSized<MyType>> for MyType`).
#[opaque_typedef(derive(AsciiExt, AsMut(Deref), AsRef(Deref), Binary, Deref, DerefMut, Display,
                        FromInner, LowerHex, Octal, PartialOrdSelf, UpperHex))]
#[opaque_typedef(cmp(partial_ord = "(|a, b| PartialOrd::partial_cmp(a, b).map(|o| o.reverse()))"))]
#[opaque_typedef(allow_mut_ref)]
pub struct ReverseOrderSized<T>(pub T);
