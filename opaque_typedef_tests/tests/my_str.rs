//! Tests for `my_str` types.

extern crate opaque_typedef;
extern crate opaque_typedef_tests;

use opaque_typedef_tests::my_str::{MyStr, MyString};


mod my_str {
    use super::*;

    fn as_inner(s: &MyStr) -> &str {
        ::opaque_typedef::OpaqueTypedefUnsized::as_inner(s)
    }

    fn ensure_eq_inner(x: &str, y: &MyStr) {
        assert_eq!(x, as_inner(y));
    }

    #[test]
    fn basic_traits() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        ensure_eq_inner(ok_str, my_str);
    }

    #[test]
    fn default_ref() {
        let ok_str = <&str as Default>::default();
        let my_str = <&MyStr as Default>::default();
        ensure_eq_inner(ok_str, my_str);
    }

    #[test]
    fn deref() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let _: &str = my_str;
    }
}

mod my_string {
    use super::*;

    fn as_inner(s: &MyString) -> &String {
        ::opaque_typedef::OpaqueTypedef::as_inner(s)
    }

    fn into_inner(s: MyString) -> String {
        ::opaque_typedef::OpaqueTypedef::into_inner(s)
    }

    #[test]
    fn basic_traits() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string.clone());
        assert_eq!(&ok_string, as_inner(&my_string));
        assert_eq!(ok_string, into_inner(my_string));
    }

    #[test]
    fn deref() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string);
        let _: &String = &my_string;
    }
}
