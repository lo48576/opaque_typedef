//! String constants.


/// `#[ATTR_NAME]`.
pub const ATTR_NAME: &str = "opaque_typedef";
/// `#[ATTR_NAME(INNER_FIELD)]`
pub const INNER_FIELD: &str = "inner";
/// `#[ATTR_NAME(DERIVE(...))]`.
pub const DERIVE: &str = "derive";
/// `#[ATTR_NAME(DEREF(...))]`.
pub const DEREF: &str = "deref";
/// `#[ATTR_NAME(DEREF(DEREF_TARGET = "foo", ...))]`.
pub const DEREF_TARGET: &str = "target";
/// `#[ATTR_NAME(DEREF(DEREF_CONV = "foo", ...))]`.
pub const DEREF_CONV: &str = "deref";
/// `#[ATTR_NAME(DEREF(DEREF_MUT_CONV = "foo", ...))]`.
pub const DEREF_CONV_MUT: &str = "deref_mut";
