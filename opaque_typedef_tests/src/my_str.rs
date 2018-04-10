//! Opaque typedef for `str`.


/// My string slice.
// Note:
//      `PartialEqSelfCowAndInner` cannot be used because it generates
//      `impl<'a> PartialEq<Cow<'a, MyStr>> for str` but `Cow` and `str` both belongs to `std` and
//      it causes error "E0117".
//      for detail, run `rustc --explain E0117`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedefUnsized)]
// About the necessity of `#[repr(C)]`, see <https://github.com/lo48576/opaque_typedef/issues/1>.
#[repr(C)]
#[opaque_typedef(
    derive(
        AsciiExt,
        AsMut(Deref, Self_),
        AsRef(Deref, Self_),
        DefaultRef,
        Deref,
        DerefMut,
        Display,
        FromInner,
        Into(Arc, Box, Rc, Inner),
        PartialEq(Inner, InnerRev, InnerCow, InnerCowRev, SelfCow, SelfCowRev),
        PartialOrd(Inner, InnerRev, InnerCow, InnerCowRev, SelfCow, SelfCowRev)
    )
)]
#[opaque_typedef(allow_mut_ref)]
pub struct MyStr {
    #[opaque_typedef(inner)]
    inner: str,
}

impl MyStr {
    /// Creates a new `&MyStr` from the given string slice.
    pub fn new(v: &str) -> &Self {
        ::opaque_typedef::OpaqueTypedefUnsized::from_inner(v)
    }

    /// Creates a new `&mut MyStr` from the given mutable string slice.
    pub fn new_mut(v: &mut str) -> &mut Self {
        ::opaque_typedef::OpaqueTypedefUnsized::from_inner_mut(v)
    }

    /// Returns a reference to the inner string slice.
    pub fn as_str(&self) -> &str {
        &self.inner
    }
}


/// My owned string.
#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, OpaqueTypedef)]
#[opaque_typedef(
    derive(
        AsMut(Deref, Inner),
        AsRef(Deref, Inner),
        Deref,
        DerefMut,
        Display,
        FromInner,
        IntoInner,
        PartialEq(Inner, InnerRev),
        PartialOrd(Inner, InnerRev)
    )
)]
#[opaque_typedef(deref(target = "str", deref = "String::as_str", deref_mut = "String::as_mut_str"))]
#[opaque_typedef(allow_mut_ref)]
pub struct MyString {
    inner: String,
}

impl MyString {
    /// Creates a new `MyString` from the given string.
    pub fn from_string(v: String) -> Self {
        ::opaque_typedef::OpaqueTypedef::from_inner(v)
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


// Implement `Borrow` and `ToOwned` to test `Cow<Mystr>`.
impl ::std::borrow::Borrow<MyStr> for MyString {
    fn borrow(&self) -> &MyStr {
        MyStr::new(self.as_str())
    }
}
impl ToOwned for MyStr {
    type Owned = MyString;
    fn to_owned(&self) -> Self::Owned {
        MyString::from_string(self.as_str().to_owned())
    }
}
