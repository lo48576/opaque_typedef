//! Tests for `atleast2items` types.

extern crate opaque_typedef;
extern crate opaque_typedef_tests;

use opaque_typedef_tests::atleast2items::{SliceAtLeast2Items, VecAtLeast2Items};


mod slice {
    use super::*;

    #[test]
    fn ok() {
        let s = &[0i32, 1];
        let v = SliceAtLeast2Items::new(s);
        assert_eq!(v.as_slice(), s);
    }

    #[test]
    #[should_panic]
    fn from_too_few_items() {
        let s = &[0i32];
        let _ = SliceAtLeast2Items::new(s);
    }

    #[test]
    #[should_panic]
    fn default() {
        // Default slice for `&[i32]` is empty (less than 2 items),
        // so this should fail.
        let _: &SliceAtLeast2Items<i32> = Default::default();
    }

    // Note:
    // Not `From<SliceAtLeast2Items<T>> for SmartPtr<[T]>` but
    // `Into<SmartPtr<[T]>> for SliceAtLeast2Items<T>` is implemented.
    // In other words, you should use `my_slice.into()` instead of
    // `SmartPtr::from(my_slice)`.
    mod convert {
        use super::*;

        #[test]
        fn into_arc() {
            let ok_slice = &[0i32, 1];
            let my_slice = SliceAtLeast2Items::new(ok_slice);
            let my_slice_arc: ::std::sync::Arc<SliceAtLeast2Items<i32>> = my_slice.into();
            let inner = <&SliceAtLeast2Items<i32> as Into<&[i32]>>::into(&*my_slice_arc);
            assert_eq!(ok_slice, inner);
        }

        #[test]
        fn into_box() {
            let ok_slice = &[0i32, 1];
            let my_slice = SliceAtLeast2Items::new(ok_slice);
            let my_slice_box: Box<SliceAtLeast2Items<i32>> = my_slice.into();
            let inner = <&SliceAtLeast2Items<i32> as Into<&[i32]>>::into(&*my_slice_box);
            assert_eq!(ok_slice, inner);
        }

        #[test]
        fn into_inner() {
            let ok_slice = &[0i32, 1];
            let my_slice = SliceAtLeast2Items::new(ok_slice);
            let inner = <&SliceAtLeast2Items<i32> as Into<&[i32]>>::into(my_slice);
            assert_eq!(ok_slice, inner);
        }

        #[test]
        fn into_rc() {
            let ok_slice = &[0i32, 1];
            let my_slice = SliceAtLeast2Items::new(ok_slice);
            let my_slice_rc: ::std::rc::Rc<SliceAtLeast2Items<i32>> = my_slice.into();
            let inner = <&SliceAtLeast2Items<i32> as Into<&[i32]>>::into(&*my_slice_rc);
            assert_eq!(ok_slice, inner);
        }

        #[test]
        fn from_inner() {
            let ok_slice: &[_] = &[0i32, 1];
            let _: &SliceAtLeast2Items<i32> = <&SliceAtLeast2Items<i32>>::from(ok_slice);
        }
    }

    // Note:
    // `PartialEq<[T]> for SliceAtLeast2Items<T>` can be implemented but
    // `PartialEq<SliceAtLeast2Items<T>> for [T]` cannot.
    // You can use inner slice exposed by `SliceAtLeast2Items::as_slice()`
    // or `AsRef::<[T]>::as_ref()` if you don't want to care operands order.
    mod cmp {
        use super::*;

        #[test]
        fn partial_eq_inner() {
            let ok_slice: &[_] = &[0i32, 1];
            let my_slice = SliceAtLeast2Items::new(ok_slice);
            assert!(<SliceAtLeast2Items<i32> as PartialEq<[i32]>>::eq(
                my_slice,
                ok_slice
            ));
            assert!(<SliceAtLeast2Items<i32> as PartialEq<&[i32]>>::eq(
                my_slice,
                &ok_slice
            ));
            assert!(<&SliceAtLeast2Items<i32> as PartialEq<[i32]>>::eq(
                &my_slice,
                ok_slice
            ));
            assert!(<&SliceAtLeast2Items<i32> as PartialEq<&[i32]>>::eq(
                &my_slice,
                &ok_slice
            ));
            //assert!(<[i32] as PartialEq<SliceAtLeast2Items<i32>>>::eq(ok_slice, my_slice));
            //assert!(<[i32] as PartialEq<&SliceAtLeast2Items<i32>>>::eq(ok_slice, &my_slice));
            //assert!(<&[i32] as PartialEq<SliceAtLeast2Items<i32>>>::eq(&ok_slice, my_slice));
            //assert!(<&[i32] as PartialEq<&SliceAtLeast2Items<i32>>>::eq(&ok_slice, &my_slice));
            // You can't `assert_eq!(ok_slice, my_slice);`, but you can
            // `assert_eq!(ok_slice, my_slice.as_slice());`.
            assert_eq!(ok_slice, my_slice.as_slice());
        }

        #[test]
        fn partial_ord_inner() {
            use std::cmp::Ordering;

            let ok_slice: &[_] = &[0i32, 1];
            let my_slice = SliceAtLeast2Items::new(ok_slice);
            assert_eq!(
                <SliceAtLeast2Items<i32> as PartialOrd<[i32]>>::partial_cmp(my_slice, ok_slice),
                Some(Ordering::Equal)
            );
            assert_eq!(
                <SliceAtLeast2Items<i32> as PartialOrd<&[i32]>>::partial_cmp(my_slice, &ok_slice),
                Some(Ordering::Equal)
            );
            assert_eq!(
                <&SliceAtLeast2Items<i32> as PartialOrd<[i32]>>::partial_cmp(&my_slice, ok_slice),
                Some(Ordering::Equal)
            );
            assert_eq!(
                <&SliceAtLeast2Items<i32> as PartialOrd<&[i32]>>::partial_cmp(&my_slice, &ok_slice),
                Some(Ordering::Equal)
            );
        }
    }

    #[test]
    fn ascii_ext() {
        use std::ascii::AsciiExt;

        let s0 = &[b'a', b'b'];
        let v0 = SliceAtLeast2Items::new(s0);
        let s1 = &[b'A', b'B'];
        let v1 = SliceAtLeast2Items::new(s1);
        assert!(v0.eq_ignore_ascii_case(v1));
    }

    mod as_ref {
        use super::*;

        #[test]
        fn as_mut() {
            let s = &mut [0i32, 1];
            let my_slice = SliceAtLeast2Items::new_mut(s);
            let _: &mut [i32] = AsMut::<[i32]>::as_mut(my_slice);
        }

        #[test]
        fn as_mut_self() {
            let s = &mut [0i32, 1];
            let my_slice = SliceAtLeast2Items::new_mut(s);
            let _: &mut SliceAtLeast2Items<i32> =
                AsMut::<SliceAtLeast2Items<i32>>::as_mut(my_slice);
        }

        #[test]
        fn as_ref() {
            let s = &[0i32, 1];
            let my_slice = SliceAtLeast2Items::new(s);
            let _: &[i32] = AsRef::<[i32]>::as_ref(my_slice);
        }

        #[test]
        fn as_ref_self() {
            let s = &[0i32, 1];
            let my_slice = SliceAtLeast2Items::new(s);
            let _: &SliceAtLeast2Items<i32> = AsRef::<SliceAtLeast2Items<i32>>::as_ref(my_slice);
        }
    }
}

mod vec {
    use super::*;

    #[test]
    fn ok() {
        let v = vec![0i32, 1];
        let my_vec = VecAtLeast2Items::from_vec(v.clone());
        assert_eq!(my_vec.as_slice(), v.as_slice());
    }

    #[test]
    #[should_panic]
    fn from_too_few_items() {
        let v = vec![0i32];
        let _ = VecAtLeast2Items::from_vec(v);
    }

    mod convert {
        use super::*;

        #[test]
        fn from_inner() {
            let ok_vec = vec![0i32, 1];
            let _: VecAtLeast2Items<i32> = <VecAtLeast2Items<i32>>::from(ok_vec);
        }

        #[test]
        fn into_inner() {
            let ok_vec = vec![0i32, 1];
            let my_vec = VecAtLeast2Items::<i32>::from_vec(ok_vec.clone());
            let inner = <VecAtLeast2Items<i32> as Into<Vec<i32>>>::into(my_vec);
            assert_eq!(ok_vec, inner);
        }
    }

    mod cmp {
        use super::*;

        #[test]
        fn partial_eq_inner() {
            let ok_vec = vec![0i32, 1];
            let my_vec = VecAtLeast2Items::from_vec(ok_vec.clone());
            assert!(<VecAtLeast2Items<i32> as PartialEq<Vec<i32>>>::eq(
                &my_vec,
                &ok_vec
            ));
            assert_eq!(ok_vec.as_slice(), my_vec.as_slice());
        }

        #[test]
        fn partial_ord_inner() {
            use std::cmp::Ordering;

            let ok_vec = vec![0i32, 1];
            let my_vec = VecAtLeast2Items::from_vec(ok_vec.clone());
            assert_eq!(
                <VecAtLeast2Items<i32> as PartialOrd<Vec<i32>>>::partial_cmp(&my_vec, &ok_vec),
                Some(Ordering::Equal)
            );
        }
    }

    // `std::ascii::AsciiExt` is deprecated, so `eq_ignore_ascii_case()` here
    // will be resolved to `slice::eq_ignore_ascii_case` by rust >= 1.23.0.
    #[test]
    #[allow(unused_imports)]
    fn ascii_ext() {
        use std::ascii::AsciiExt;

        let s0 = vec![b'a', b'b'];
        let v0 = VecAtLeast2Items::from_vec(s0);
        let s1 = vec![b'A', b'B'];
        let v1 = VecAtLeast2Items::from_vec(s1);
        assert!(v0.eq_ignore_ascii_case(&v1));
    }

    mod as_ref {
        use super::*;

        #[test]
        fn as_ref_deref() {
            let v = vec![0i32, 1];
            let my_vec = VecAtLeast2Items::from_vec(v);
            let _: &[i32] = AsRef::<[i32]>::as_ref(&my_vec);
        }

        #[test]
        fn as_ref_inner() {
            let v = vec![0i32, 1];
            let my_vec = VecAtLeast2Items::from_vec(v);
            let _: &Vec<i32> = AsRef::<Vec<i32>>::as_ref(&my_vec);
        }
    }
}
