//! Opaque typedef for `str`.


/// My string slice.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
#[opaque_typedef(derive(AsciiExt, AsMutDeref, AsMutSelf, AsRefDeref, AsRefSelf, DefaultRef,
                        Deref, DerefMut, Display, FromInner, IntoArc, IntoBox, IntoRc, IntoInner))]
#[opaque_typedef(allow_mut_ref)]
pub struct MyStr {
    #[opaque_typedef(inner)] inner: str,
}

impl MyStr {
    /// Creates a new `&MyStr` from the given string slice.
    pub fn new(v: &str) -> &Self {
        ::opaque_typedef::OpaqueTypedefUnsized::from_inner(v).unwrap()
    }

    /// Creates a new `&mut MyStr` from the given mutable string slice.
    pub fn new_mut(v: &mut str) -> &mut Self {
        ::opaque_typedef::OpaqueTypedefUnsized::from_inner_mut(v).unwrap()
    }

    /// Returns a reference to the inner string slice.
    pub fn as_str(&self) -> &str {
        &self.inner
    }
}


/// My owned string.
#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(derive(AsMutDeref, AsMutInner, AsRefDeref, AsRefInner, Deref, DerefMut,
                        Display, FromInner, IntoInner))]
#[opaque_typedef(deref(target = "str", deref = "MyString::as_str",
                       deref_mut = "MyString::as_mut_str"))]
#[opaque_typedef(allow_mut_ref)]
pub struct MyString {
    inner: String,
}

impl MyString {
    /// Creates a new `MyString` from the given string.
    pub fn from_string(v: String) -> Self {
        ::opaque_typedef::OpaqueTypedef::from_inner(v).unwrap()
    }

    /// Returns a reference to the inner string slice.
    pub fn as_str(&self) -> &str {
        &self.inner
    }

    /// Returns a mutable reference to the inner string slice.
    pub fn as_mut_str(&mut self) -> &mut str {
        self.inner.as_mut_str()
    }
}
