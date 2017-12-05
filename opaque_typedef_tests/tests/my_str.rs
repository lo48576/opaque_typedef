//! Tests for `my_str` types.

extern crate opaque_typedef;
extern crate opaque_typedef_tests;

use opaque_typedef_tests::my_str::{MyStr, MyString};


mod my_str {
    use super::*;

    fn sys_as_inner(s: &MyStr) -> &str {
        ::opaque_typedef::OpaqueTypedefUnsized::as_inner(s)
    }

    fn ensure_eq_inner(x: &str, y: &MyStr) {
        assert_eq!(x, sys_as_inner(y));
    }

    #[test]
    fn basic_traits() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        ensure_eq_inner(ok_str, my_str);
    }

    #[test]
    fn ascii_ext() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let ok_upper = "FOOBAR";
        let my_str_upper = MyStr::new(ok_upper);
        assert!(<MyStr as ::std::ascii::AsciiExt>::eq_ignore_ascii_case(
            my_str,
            my_str_upper
        ));
    }

    #[test]
    fn as_mut() {
        let mut ok_string = "foobar".to_owned();
        let ok_str: &mut str = &mut ok_string;
        let my_str: &mut MyStr = MyStr::new_mut(ok_str);
        let _: &mut str = AsMut::<str>::as_mut(my_str);
    }

    #[test]
    fn as_mut_self() {
        let mut ok_string = "foobar".to_owned();
        let ok_str: &mut str = &mut ok_string;
        let my_str: &mut MyStr = MyStr::new_mut(ok_str);
        let _: &mut MyStr = AsMut::<MyStr>::as_mut(my_str);
    }

    #[test]
    fn as_ref() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let _: &str = AsRef::<str>::as_ref(my_str);
    }

    #[test]
    fn as_ref_self() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let _: &MyStr = AsRef::<MyStr>::as_ref(my_str);
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

    #[test]
    fn deref_mut() {
        let mut ok_string = "foobar".to_owned();
        let ok_str: &mut str = &mut ok_string;
        let my_str: &mut MyStr = MyStr::new_mut(ok_str);
        let _: &mut str = my_str;
    }

    #[test]
    fn display() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        assert_eq!(format!("{}", ok_str), format!("{}", my_str));
    }

    #[test]
    fn from_inner() {
        let ok_str = "foobar";
        let _: &MyStr = <&MyStr as From<&str>>::from(ok_str);
    }

    #[test]
    fn into_arc() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let my_str_arc = ::std::sync::Arc::<MyStr>::from(my_str);
        let inner = <&MyStr as Into<&str>>::into(&*my_str_arc);
        assert_eq!(ok_str, inner);
    }

    #[test]
    fn into_box() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let my_str_box = Box::<MyStr>::from(my_str);
        let inner = <&MyStr as Into<&str>>::into(&*my_str_box);
        assert_eq!(ok_str, inner);
    }

    #[test]
    fn into_rc() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let my_str_rc = ::std::rc::Rc::<MyStr>::from(my_str);
        let inner = <&MyStr as Into<&str>>::into(&*my_str_rc);
        assert_eq!(ok_str, inner);
    }

    #[test]
    fn into_inner() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let inner = <&MyStr as Into<&str>>::into(my_str);
        assert_eq!(ok_str, inner);
    }

    #[test]
    fn partial_eq_inner() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        assert!(<MyStr as PartialEq<str>>::eq(my_str, ok_str));
        assert!(<MyStr as PartialEq<&str>>::eq(my_str, &ok_str));
        assert!(<&MyStr as PartialEq<str>>::eq(&my_str, ok_str));
        assert!(<&MyStr as PartialEq<&str>>::eq(&my_str, &ok_str));
        assert!(<str as PartialEq<MyStr>>::eq(ok_str, my_str));
        assert!(<str as PartialEq<&MyStr>>::eq(ok_str, &my_str));
        assert!(<&str as PartialEq<MyStr>>::eq(&ok_str, my_str));
        assert!(<&str as PartialEq<&MyStr>>::eq(&ok_str, &my_str));
    }

    /*
    // `str` is the inner type of `MyStr`.
    // `OsStr` is the inner type of `Path`.
    // So the assertions like the below should be successful for `str` and `MyStr`.
    #[test]
    fn sample_partial_eq_path() {
        use std::path::Path;
        use std::ffi::OsStr;
        let ok_str = OsStr::new("foobar");
        let my_str = Path::new(ok_str);
        assert!(<Path as PartialEq<OsStr>>::eq(my_str, ok_str));
        assert!(<Path as PartialEq<&OsStr>>::eq(my_str, &ok_str));
        assert!(<&Path as PartialEq<OsStr>>::eq(&my_str, ok_str));
        assert!(<&Path as PartialEq<&OsStr>>::eq(&my_str, &ok_str));
        assert!(<OsStr as PartialEq<Path>>::eq(ok_str, my_str));
        assert!(<OsStr as PartialEq<&Path>>::eq(ok_str, &my_str));
        assert!(<&OsStr as PartialEq<Path>>::eq(&ok_str, my_str));
        assert!(<&OsStr as PartialEq<&Path>>::eq(&ok_str, &my_str));
    }
    */

    #[test]
    fn partial_eq_inner_cow() {
        use std::borrow::Cow;

        let ok_str = "foobar";
        let ok_cow = Cow::Borrowed(ok_str);
        let my_str = MyStr::new(ok_str);
        assert!(<MyStr as PartialEq<Cow<str>>>::eq(my_str, &ok_cow));
        assert!(<&MyStr as PartialEq<Cow<str>>>::eq(&my_str, &ok_cow));
        assert!(<Cow<str> as PartialEq<MyStr>>::eq(&ok_cow, my_str));
        assert!(<Cow<str> as PartialEq<&MyStr>>::eq(&ok_cow, &my_str));
    }

    #[test]
    fn partial_eq_self_cow() {
        use std::borrow::Cow;

        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let my_cow = Cow::Borrowed(my_str);
        assert!(<MyStr as PartialEq<Cow<MyStr>>>::eq(my_str, &my_cow));
        assert!(<&MyStr as PartialEq<Cow<MyStr>>>::eq(&my_str, &my_cow));
        assert!(<Cow<MyStr> as PartialEq<MyStr>>::eq(&my_cow, my_str));
        assert!(<Cow<MyStr> as PartialEq<&MyStr>>::eq(&my_cow, &my_str));
    }

    #[test]
    fn partial_ord_inner() {
        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <MyStr as PartialOrd<str>>::partial_cmp(my_str, ok_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <MyStr as PartialOrd<&str>>::partial_cmp(my_str, &ok_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <&MyStr as PartialOrd<str>>::partial_cmp(&my_str, ok_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <&MyStr as PartialOrd<&str>>::partial_cmp(&my_str, &ok_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <str as PartialOrd<MyStr>>::partial_cmp(ok_str, my_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <str as PartialOrd<&MyStr>>::partial_cmp(ok_str, &my_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <&str as PartialOrd<MyStr>>::partial_cmp(&ok_str, my_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <&str as PartialOrd<&MyStr>>::partial_cmp(&ok_str, &my_str)
        );
    }

    #[test]
    fn partial_ord_inner_cow() {
        use std::borrow::Cow;

        let ok_str = "foobar";
        let ok_cow = Cow::Borrowed(ok_str);
        let my_str = MyStr::new(ok_str);
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <MyStr as PartialOrd<Cow<str>>>::partial_cmp(my_str, &ok_cow)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <&MyStr as PartialOrd<Cow<str>>>::partial_cmp(&my_str, &ok_cow)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <Cow<str> as PartialOrd<MyStr>>::partial_cmp(&ok_cow, my_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <Cow<str> as PartialOrd<&MyStr>>::partial_cmp(&ok_cow, &my_str)
        );
    }

    #[test]
    fn partial_ord_self_cow() {
        use std::borrow::Cow;

        let ok_str = "foobar";
        let my_str = MyStr::new(ok_str);
        let my_cow = Cow::Borrowed(my_str);
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <MyStr as PartialOrd<Cow<MyStr>>>::partial_cmp(my_str, &my_cow)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <&MyStr as PartialOrd<Cow<MyStr>>>::partial_cmp(&my_str, &my_cow)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <Cow<MyStr> as PartialOrd<MyStr>>::partial_cmp(&my_cow, my_str)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <Cow<MyStr> as PartialOrd<&MyStr>>::partial_cmp(&my_cow, &my_str)
        );
    }
}

mod my_string {
    use super::*;

    fn sys_as_inner(s: &MyString) -> &String {
        ::opaque_typedef::OpaqueTypedef::as_inner(s)
    }

    fn sys_into_inner(s: MyString) -> String {
        ::opaque_typedef::OpaqueTypedef::into_inner(s)
    }

    #[test]
    fn basic_traits() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string.clone());
        assert_eq!(&ok_string, sys_as_inner(&my_string));
        assert_eq!(ok_string, sys_into_inner(my_string));
    }

    #[test]
    fn as_ref_deref() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string);
        let _: &str = AsRef::<str>::as_ref(&my_string);
    }

    #[test]
    fn as_ref_inner() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string);
        let _: &String = AsRef::<String>::as_ref(&my_string);
    }

    #[test]
    fn as_mut_deref() {
        let ok_string = "foobar".to_owned();
        let mut my_string = MyString::from_string(ok_string);
        let _: &mut str = AsMut::<str>::as_mut(&mut my_string);
    }

    #[test]
    fn as_mut_inner() {
        let ok_string = "foobar".to_owned();
        let mut my_string = MyString::from_string(ok_string);
        let _: &mut String = AsMut::<String>::as_mut(&mut my_string);
    }

    #[test]
    fn deref() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string);
        let _: &str = &my_string;
    }

    #[test]
    fn display() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string.clone());
        assert_eq!(format!("{}", ok_string), format!("{}", my_string));
    }

    #[test]
    fn deref_mut() {
        let ok_string = "foobar".to_owned();
        let mut my_string = MyString::from_string(ok_string);
        let _: &mut str = &mut my_string;
    }

    #[test]
    fn from_inner() {
        let ok_string = "foobar".to_owned();
        let _: MyString = <MyString as From<String>>::from(ok_string);
    }

    #[test]
    fn into_inner() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string.clone());
        let inner = <MyString as Into<String>>::into(my_string);
        assert_eq!(ok_string, inner);
    }

    #[test]
    fn partial_eq_inner() {
        let ok_string = "foobar".to_owned();
        let my_string = MyString::from_string(ok_string.clone());
        assert!(<MyString as PartialEq<String>>::eq(&my_string, &ok_string));
        assert!(<MyString as PartialEq<&String>>::eq(
            &my_string,
            &&ok_string
        ));
        assert!(<&MyString as PartialEq<String>>::eq(
            &&my_string,
            &ok_string
        ));
        assert!(<&MyString as PartialEq<&String>>::eq(
            &&my_string,
            &&ok_string
        ));
        assert!(<String as PartialEq<MyString>>::eq(&ok_string, &my_string));
        assert!(<String as PartialEq<&MyString>>::eq(
            &ok_string,
            &&my_string
        ));
        assert!(<&String as PartialEq<MyString>>::eq(
            &&ok_string,
            &my_string
        ));
        assert!(<&String as PartialEq<&MyString>>::eq(
            &&ok_string,
            &&my_string
        ));
    }
}
