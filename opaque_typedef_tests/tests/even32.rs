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
    fn binary() {
        let v = Even32::from_i32(42);
        assert_eq!(format!("{:b}", v), "101010");
        assert_eq!(format!("{:#b}", v), "0b101010");
    }

    #[test]
    fn display() {
        let v = Even32::from_i32(42);
        assert_eq!(format!("{}", v), "42");
    }

    #[test]
    fn lower_hex() {
        let v = Even32::from_i32(42);
        assert_eq!(format!("{:x}", v), "2a");
        assert_eq!(format!("{:#x}", v), "0x2a");
    }

    #[test]
    fn octal() {
        let v = Even32::from_i32(42);
        assert_eq!(format!("{:o}", v), "52");
        assert_eq!(format!("{:#o}", v), "0o52");
    }

    #[test]
    fn upper_hex() {
        let v = Even32::from_i32(42);
        assert_eq!(format!("{:X}", v), "2A");
        assert_eq!(format!("{:#X}", v), "0x2A");
    }
}
