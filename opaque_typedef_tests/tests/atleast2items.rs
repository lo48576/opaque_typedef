//! Tests for `atleast2items` types.

extern crate opaque_typedef;
extern crate opaque_typedef_tests;

use opaque_typedef_tests::atleast2items::SliceAtLeast2Items;


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

    // Note:
    // Not `From<SliceAtLeast2Items<T>> for SmartPtr<[T]>` but
    // `Into<SmartPtr<[T]>> for SliceAtLeast2Items<T>` is implemented.
    // In other words, you should use `my_slice.into()` instead of
    // `SmartPtr::from(my_slice)`.
    mod into_smartptr {
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
    }
}
