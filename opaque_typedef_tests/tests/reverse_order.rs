//! A wrapper type with reverse order.

extern crate opaque_typedef;
extern crate opaque_typedef_tests;

use opaque_typedef_tests::reverse_order::ReverseOrderSized;


mod sized {
    use super::*;

    #[test]
    fn from_inner() {
        let _: ReverseOrderSized<i32> = ReverseOrderSized::<i32>::from(0i32);
        let _: ReverseOrderSized<i32> = 0i32.into();
    }

    mod partial_ord {
        use super::*;

        fn assert_reverse_cmp<T, U>(t: T, u: U)
        where
            T: PartialOrd<U>,
            ReverseOrderSized<T>: PartialOrd<ReverseOrderSized<U>>,
        {
            let orig_result = PartialOrd::partial_cmp(&t, &u);
            let wrapped_result =
                PartialOrd::partial_cmp(&ReverseOrderSized(t), &ReverseOrderSized(u));
            assert_eq!(orig_result.map(|o| o.reverse()), wrapped_result);
        }

        #[test]
        fn int32() {
            assert_reverse_cmp(3i32, 2i32);
            assert_reverse_cmp(3i32, 3i32);
            assert_reverse_cmp(3i32, 4i32);
        }

        #[test]
        fn string() {
            assert_reverse_cmp("hello", "abc");
            assert_reverse_cmp("hello", "hello");
            assert_reverse_cmp("hello", "world");
        }
    }

    mod ord {
        use super::*;

        fn assert_reverse_cmp<T>(t: T, u: T)
        where
            T: Ord,
            ReverseOrderSized<T>: Ord,
        {
            let orig_result = Ord::cmp(&t, &u);
            let wrapped_result = Ord::cmp(&ReverseOrderSized(t), &ReverseOrderSized(u));
            assert_eq!(orig_result.reverse(), wrapped_result);
        }

        #[test]
        fn int32() {
            assert_reverse_cmp(3i32, 2i32);
            assert_reverse_cmp(3i32, 3i32);
            assert_reverse_cmp(3i32, 4i32);
        }

        #[test]
        fn string() {
            assert_reverse_cmp("hello", "abc");
            assert_reverse_cmp("hello", "hello");
            assert_reverse_cmp("hello", "world");
        }
    }

    mod ops {
        use super::*;

        #[test]
        fn add() {
            let raw_x = 10;
            let x = ReverseOrderSized::from(raw_x);
            let raw_y = 32;
            let y = ReverseOrderSized::from(raw_y);
            let raw_sum = raw_x + raw_y;
            let sum = ReverseOrderSized::from(raw_sum);
            // raw_raw
            assert_eq!(x + y, sum);
            assert_eq!(x + raw_y, sum);
            //assert_eq!(raw_x + y, sum);
        }

        #[test]
        fn add_ref() {
            let raw_x = 10;
            let x = ReverseOrderSized::from(raw_x);
            let raw_y = 32;
            let y = ReverseOrderSized::from(raw_y);
            let raw_sum = raw_x + raw_y;
            let sum = ReverseOrderSized::from(raw_sum);
            // raw_ref
            assert_eq!(x + &y, sum);
            assert_eq!(x + &raw_y, sum);
            // ref_raw
            assert_eq!(&x + y, sum);
            assert_eq!(&x + raw_y, sum);
            // ref_ref
            assert_eq!(&x + &y, sum);
            assert_eq!(&x + &raw_y, sum);
        }

        #[test]
        fn add_assign() {
            let raw_x = 10;
            let x = ReverseOrderSized::from(raw_x);
            let raw_y = 32;
            let y = ReverseOrderSized::from(raw_y);
            let raw_sum = raw_x + raw_y;
            let sum = ReverseOrderSized::from(raw_sum);
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

        #[test]
        fn not() {
            let raw_x = 42;
            let x = ReverseOrderSized::from(raw_x);
            let raw_result = !raw_x;
            let result = ReverseOrderSized::from(raw_result);
            // raw
            assert_eq!(!x, result);
        }

        #[test]
        fn not_ref() {
            let raw_x = 42;
            let x = ReverseOrderSized::from(raw_x);
            let raw_result = !raw_x;
            let result = ReverseOrderSized::from(raw_result);
            // ref
            assert_eq!(!(&x), result);
        }
    }
}
