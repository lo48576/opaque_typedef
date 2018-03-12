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
}
