//! All same.

extern crate opaque_typedef;
extern crate opaque_typedef_tests;

use opaque_typedef_tests::all_eq::AllEq;


mod sized {
    use super::*;

    #[test]
    fn from_inner() {
        let _: AllEq<i32> = AllEq::<i32>::from(0i32);
        let _: AllEq<i32> = 0i32.into();
    }

    mod partial_eq {
        use super::*;

        fn assert_all_eq<T, U>(t: T, u: U)
        where
            AllEq<T>: PartialEq<AllEq<U>>,
        {
            let result = PartialEq::eq(&AllEq(t), &AllEq(u));
            assert_eq!(result, true);
        }

        #[test]
        fn int32() {
            assert_all_eq(3i32, 2i32);
            assert_all_eq(3i32, 3i32);
            assert_all_eq(3i32, 4i32);
        }

        #[test]
        fn string() {
            assert_all_eq("hello", "abc");
            assert_all_eq("hello", "hello");
            assert_all_eq("hello", "world");
        }
    }
}
