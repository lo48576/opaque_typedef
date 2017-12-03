//! Tests for `even32` types.

extern crate opaque_typedef;
extern crate opaque_typedef_tests;

use opaque_typedef_tests::even32::Even32;


mod basic {
    use super::*;

    #[test]
    fn ok() {
        let v = Even32::from_i32(42);
        assert_eq!(v.to_i32(), 42);
    }

    #[test]
    #[should_panic]
    fn from_odd() {
        let _ = Even32::from_i32(3);
    }
}

mod fmt {
    use super::*;

    #[test]
    fn display() {
        let v = Even32::from_i32(42);
        assert_eq!(format!("{}", v), "42");
    }
}
