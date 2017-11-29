//! Opaque typedef for `str`.


/// My string slice.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
#[opaque_typedef(derive(AsRef, DefaultRef, Deref, DerefMut))]
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
#[opaque_typedef(derive(AsRef, Deref, DerefMut))]
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
}
