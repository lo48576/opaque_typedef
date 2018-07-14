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

    macro_rules! test_binop_num {
        ($testname:ident, $x:expr, $y:expr, $method:expr,direct) => {
            #[test]
            fn $testname() {
                let raw_x = $x;
                let x = Int32::from(raw_x);
                let raw_y = $y;
                let y = Int32::from(raw_y);
                let raw_result = $method(raw_x, raw_y);
                let result = Int32::from(raw_result);
                // raw_raw
                assert_eq!($method(x, y), result);
                assert_eq!($method(x, raw_y), result);
                assert_eq!($method(raw_x, y), result);
            }
        };
        ($testname:ident, $x:expr, $y:expr, $method:expr,references) => {
            #[test]
            fn $testname() {
                let raw_x = $x;
                let x = Int32::from(raw_x);
                let raw_y = $y;
                let y = Int32::from(raw_y);
                let raw_result = $method(raw_x, raw_y);
                let result = Int32::from(raw_result);
                // raw_ref
                assert_eq!($method(x, &y), result);
                assert_eq!($method(x, &raw_y), result);
                assert_eq!($method(raw_x, &y), result);
                // ref_raw
                assert_eq!($method(&x, y), result);
                assert_eq!($method(&x, raw_y), result);
                assert_eq!($method(&raw_x, y), result);
                // ref_ref
                assert_eq!($method(&x, &y), result);
                assert_eq!($method(&x, &raw_y), result);
                assert_eq!($method(&raw_x, &y), result);
            }
        };
    }

    // Arithmetic.
    test_binop_num!(add, 10, 32, ::std::ops::Add::add, direct);
    test_binop_num!(add_ref, 10, 32, ::std::ops::Add::add, references);
    test_binop_num!(div, 32, 10, ::std::ops::Div::div, direct);
    test_binop_num!(div_ref, 32, 10, ::std::ops::Div::div, references);
    test_binop_num!(mul, 10, 32, ::std::ops::Mul::mul, direct);
    test_binop_num!(mul_ref, 10, 32, ::std::ops::Mul::mul, references);
    test_binop_num!(rem, 32, 10, ::std::ops::Rem::rem, direct);
    test_binop_num!(rem_ref, 32, 10, ::std::ops::Rem::rem, references);
    test_binop_num!(sub, 10, 32, ::std::ops::Sub::sub, direct);
    test_binop_num!(sub_ref, 10, 32, ::std::ops::Sub::sub, references);
    test_binop_num!(shl, 8191, 8, ::std::ops::Shl::shl, direct);
    test_binop_num!(shl_ref, 8191, 8, ::std::ops::Shl::shl, references);
    test_binop_num!(shr, 8191, 8, ::std::ops::Shr::shr, direct);
    test_binop_num!(shr_ref, 8191, 8, ::std::ops::Shr::shr, references);
    // Bitwise.
    test_binop_num!(bitand, 10, 32, ::std::ops::BitAnd::bitand, direct);
    test_binop_num!(bitand_ref, 10, 32, ::std::ops::BitAnd::bitand, references);
    test_binop_num!(bitor, 10, 32, ::std::ops::BitAnd::bitand, direct);
    test_binop_num!(bitor_ref, 10, 32, ::std::ops::BitAnd::bitand, references);
    test_binop_num!(bitxor, 10, 32, ::std::ops::BitAnd::bitand, direct);
    test_binop_num!(bitxor_ref, 10, 32, ::std::ops::BitAnd::bitand, references);

    macro_rules! test_unaryop_num {
        ($testname:ident, $x:expr, $method:expr,direct) => {
            #[test]
            fn $testname() {
                let raw_x = $x;
                let x = Int32::from(raw_x);
                let raw_result = $method(raw_x);
                let result = Int32::from(raw_result);
                // raw
                assert_eq!($method(x), result);
            }
        };
        ($testname:ident, $x:expr, $method:expr,references) => {
            #[test]
            fn $testname() {
                let raw_x = $x;
                let x = Int32::from(raw_x);
                let raw_result = $method(raw_x);
                let result = Int32::from(raw_result);
                // ref
                assert_eq!($method(&x), result);
            }
        };
    }

    test_unaryop_num!(neg, 42, ::std::ops::Neg::neg, direct);
    test_unaryop_num!(neg_ref, 42, ::std::ops::Neg::neg, references);
    test_unaryop_num!(not, 42, ::std::ops::Not::not, direct);
    test_unaryop_num!(not_ref, 42, ::std::ops::Not::not, references);

    macro_rules! test_binop_num_assign {
        ($testname:ident, $x:expr, $y:expr, $method:expr,direct) => {
            #[test]
            fn $testname() {
                let raw_x = $x;
                let x = Int32::from(raw_x);
                let raw_y = $y;
                let y = Int32::from(raw_y);
                let mut raw_result = raw_x;
                $method(&mut raw_result, raw_y);
                let result = Int32::from(raw_result);
                // raw_raw
                {
                    let mut x = x;
                    $method(&mut x, y);
                    assert_eq!(x, result);
                }
                {
                    let mut x = x;
                    $method(&mut x, raw_y);
                    assert_eq!(x, result);
                }
            }
        };
        ($testname:ident, $x:expr, $y:expr, $method:expr,references) => {
            #[test]
            fn $testname() {
                let raw_x = $x;
                let x = Int32::from(raw_x);
                let raw_y = $y;
                let y = Int32::from(raw_y);
                let mut raw_result = raw_x;
                $method(&mut raw_result, raw_y);
                let result = Int32::from(raw_result);
                // raw_ref
                {
                    let mut x = x;
                    $method(&mut x, &y);
                    assert_eq!(x, result);
                }
                {
                    let mut x = x;
                    $method(&mut x, &raw_y);
                    assert_eq!(x, result);
                }
            }
        };
    }

    // Arithmetic.
    test_binop_num_assign!(
        add_assign,
        10,
        32,
        ::std::ops::AddAssign::add_assign,
        direct
    );
    test_binop_num_assign!(
        add_assign_ref,
        10,
        32,
        ::std::ops::AddAssign::add_assign,
        references
    );
    test_binop_num_assign!(
        div_assign,
        32,
        10,
        ::std::ops::DivAssign::div_assign,
        direct
    );
    test_binop_num_assign!(
        div_assign_ref,
        32,
        10,
        ::std::ops::DivAssign::div_assign,
        references
    );
    test_binop_num_assign!(
        mul_assign,
        10,
        32,
        ::std::ops::MulAssign::mul_assign,
        direct
    );
    test_binop_num_assign!(
        mul_assign_ref,
        10,
        32,
        ::std::ops::MulAssign::mul_assign,
        references
    );
    test_binop_num_assign!(
        rem_assign,
        32,
        10,
        ::std::ops::RemAssign::rem_assign,
        direct
    );
    test_binop_num_assign!(
        rem_assign_ref,
        32,
        10,
        ::std::ops::RemAssign::rem_assign,
        references
    );
    test_binop_num_assign!(
        sub_assign,
        10,
        32,
        ::std::ops::SubAssign::sub_assign,
        direct
    );
    test_binop_num_assign!(
        sub_assign_ref,
        10,
        32,
        ::std::ops::SubAssign::sub_assign,
        references
    );

    // Bitwise.
    test_binop_num_assign!(
        bitand_assign,
        10,
        32,
        ::std::ops::BitAndAssign::bitand_assign,
        direct
    );
    test_binop_num_assign!(
        bitand_assign_ref,
        10,
        32,
        ::std::ops::BitAndAssign::bitand_assign,
        references
    );
    test_binop_num_assign!(
        bitor_assign,
        10,
        32,
        ::std::ops::BitOrAssign::bitor_assign,
        direct
    );
    test_binop_num_assign!(
        bitor_assign_ref,
        10,
        32,
        ::std::ops::BitOrAssign::bitor_assign,
        references
    );
    test_binop_num_assign!(
        bitxor_assign,
        10,
        32,
        ::std::ops::BitXorAssign::bitxor_assign,
        direct
    );
    test_binop_num_assign!(
        bitxor_assign_ref,
        10,
        32,
        ::std::ops::BitXorAssign::bitxor_assign,
        references
    );
    test_binop_num_assign!(
        shl_assign,
        8191,
        8,
        ::std::ops::ShlAssign::shl_assign,
        direct
    );
    test_binop_num_assign!(
        shl_assign_ref,
        8191,
        8,
        ::std::ops::ShlAssign::shl_assign,
        references
    );
    test_binop_num_assign!(
        shr_assign,
        8191,
        8,
        ::std::ops::ShrAssign::shr_assign,
        direct
    );
    test_binop_num_assign!(
        shr_assign_ref,
        8191,
        8,
        ::std::ops::ShrAssign::shr_assign,
        references
    );
}
