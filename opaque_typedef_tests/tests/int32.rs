//! Tests for `int32` types.

extern crate opaque_typedef;
extern crate opaque_typedef_tests;

use opaque_typedef_tests::int32::Int32;


mod basic {
    use super::*;

    #[test]
    fn ok() {
        let v = Int32::from(42);
        assert_eq!(v.to_i32(), 42);
    }
}

mod cmp {
    use super::*;

    #[test]
    fn partial_eq_inner() {
        let i = 42i32;
        let v = Int32::from(42);
        assert!(<Int32 as PartialEq<i32>>::eq(&v, &i));
        assert!(<Int32 as PartialEq<&i32>>::eq(&v, &&i));
        assert!(<&Int32 as PartialEq<i32>>::eq(&&v, &i));
        assert!(<i32 as PartialEq<Int32>>::eq(&i, &v));
        assert!(<i32 as PartialEq<&Int32>>::eq(&i, &&v));
        assert!(<&i32 as PartialEq<Int32>>::eq(&&i, &v));
        let different = 2i32;
        assert!(!<Int32 as PartialEq<i32>>::eq(&v, &different));
        assert!(!<Int32 as PartialEq<&i32>>::eq(&v, &&different));
        assert!(!<&Int32 as PartialEq<i32>>::eq(&&v, &different));
        assert!(!<i32 as PartialEq<Int32>>::eq(&different, &v));
        assert!(!<i32 as PartialEq<&Int32>>::eq(&different, &&v));
        assert!(!<&i32 as PartialEq<Int32>>::eq(&&different, &v));
    }

    #[test]
    fn partial_ord_inner() {
        let i = 42i32;
        let v = Int32::from(42);
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <Int32 as PartialOrd<i32>>::partial_cmp(&v, &i)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <Int32 as PartialOrd<&i32>>::partial_cmp(&v, &&i)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <&Int32 as PartialOrd<i32>>::partial_cmp(&&v, &i)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <i32 as PartialOrd<Int32>>::partial_cmp(&i, &v)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <i32 as PartialOrd<&Int32>>::partial_cmp(&i, &&v)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Equal),
            <&i32 as PartialOrd<Int32>>::partial_cmp(&&i, &v)
        );
        let less = 2i32;
        assert_eq!(
            Some(::std::cmp::Ordering::Greater),
            <Int32 as PartialOrd<i32>>::partial_cmp(&v, &less)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Greater),
            <Int32 as PartialOrd<&i32>>::partial_cmp(&v, &&less)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Greater),
            <&Int32 as PartialOrd<i32>>::partial_cmp(&&v, &less)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Less),
            <i32 as PartialOrd<Int32>>::partial_cmp(&less, &v)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Less),
            <i32 as PartialOrd<&Int32>>::partial_cmp(&less, &&v)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Less),
            <&i32 as PartialOrd<Int32>>::partial_cmp(&&less, &v)
        );
        let greater = 128i32;
        assert_eq!(
            Some(::std::cmp::Ordering::Less),
            <Int32 as PartialOrd<i32>>::partial_cmp(&v, &greater)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Less),
            <Int32 as PartialOrd<&i32>>::partial_cmp(&v, &&greater)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Less),
            <&Int32 as PartialOrd<i32>>::partial_cmp(&&v, &greater)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Greater),
            <i32 as PartialOrd<Int32>>::partial_cmp(&greater, &v)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Greater),
            <i32 as PartialOrd<&Int32>>::partial_cmp(&greater, &&v)
        );
        assert_eq!(
            Some(::std::cmp::Ordering::Greater),
            <&i32 as PartialOrd<Int32>>::partial_cmp(&&greater, &v)
        );
    }
}

mod fmt {
    use super::*;

    #[test]
    fn binary() {
        let v = Int32::from(42);
        assert_eq!(format!("{:b}", v), "101010");
        assert_eq!(format!("{:#b}", v), "0b101010");
    }

    #[test]
    fn display() {
        let v = Int32::from(42);
        assert_eq!(format!("{}", v), "42");
    }

    #[test]
    fn lower_hex() {
        let v = Int32::from(42);
        assert_eq!(format!("{:x}", v), "2a");
        assert_eq!(format!("{:#x}", v), "0x2a");
    }

    #[test]
    fn octal() {
        let v = Int32::from(42);
        assert_eq!(format!("{:o}", v), "52");
        assert_eq!(format!("{:#o}", v), "0o52");
    }

    #[test]
    fn upper_hex() {
        let v = Int32::from(42);
        assert_eq!(format!("{:X}", v), "2A");
        assert_eq!(format!("{:#X}", v), "0x2A");
    }
}

mod convert {
    use super::*;

    #[test]
    fn as_ref_deref() {
        let x = Int32::from(10);
        let _: &i32 = AsRef::<i32>::as_ref(&x);
    }

    #[test]
    fn as_ref_self() {
        let x = Int32::from(10);
        let _: &Int32 = AsRef::<Int32>::as_ref(&x);
    }

    #[test]
    fn as_mut_deref() {
        let mut x = Int32::from(10);
        let raw_y = 42;
        *AsMut::<i32>::as_mut(&mut x) = raw_y;
        assert_eq!(x.to_i32(), raw_y);
    }

    #[test]
    fn as_mut_self() {
        let mut x = Int32::from(10);
        let raw_y = 42;
        let y = Int32::from(raw_y);
        *AsMut::<Int32>::as_mut(&mut x) = y;
        assert_eq!(x.to_i32(), raw_y);
    }
}

mod ops {
    use super::*;

    #[test]
    fn add() {
        let raw_x = 10;
        let x = Int32::from(raw_x);
        let raw_y = 32;
        let y = Int32::from(raw_y);
        let raw_sum = raw_x + raw_y;
        let sum = Int32::from(raw_sum);
        // raw_raw
        assert_eq!(x + y, sum);
        assert_eq!(x + raw_y, sum);
        assert_eq!(raw_x + y, sum);
    }

    #[test]
    fn add_ref() {
        let raw_x = 10;
        let x = Int32::from(raw_x);
        let raw_y = 32;
        let y = Int32::from(raw_y);
        let raw_sum = raw_x + raw_y;
        let sum = Int32::from(raw_sum);
        // raw_ref
        assert_eq!(x + &y, sum);
        assert_eq!(x + &raw_y, sum);
        assert_eq!(raw_x + &y, sum);
        // ref_raw
        assert_eq!(&x + y, sum);
        assert_eq!(&x + raw_y, sum);
        assert_eq!(&raw_x + y, sum);
        // ref_ref
        assert_eq!(&x + &y, sum);
        assert_eq!(&x + &raw_y, sum);
        assert_eq!(&raw_x + &y, sum);
    }

    #[test]
    fn add_assign() {
        let raw_x = 10;
        let x = Int32::from(raw_x);
        let raw_y = 32;
        let y = Int32::from(raw_y);
        let raw_sum = raw_x + raw_y;
        let sum = Int32::from(raw_sum);
        // raw_raw
        {
            let mut x = x;
            x += y;
            assert_eq!(x, sum);
        }
        {
            let mut x = x;
            x += raw_y;
            assert_eq!(x, sum);
        }
    }

    #[test]
    fn add_assign_ref() {
        let raw_x = 10;
        let x = Int32::from(raw_x);
        let raw_y = 32;
        let y = Int32::from(raw_y);
        let raw_sum = raw_x + raw_y;
        let sum = Int32::from(raw_sum);
        // raw_ref
        {
            let mut x = x;
            x += &y;
            assert_eq!(x, sum);
        }
        {
            let mut x = x;
            x += &raw_y;
            assert_eq!(x, sum);
        }
    }
}
