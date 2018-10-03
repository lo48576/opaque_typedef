//! Builders.

use std::error;
use std::fmt;

pub use self::generics::GenericsBuilder;

mod generics;

/// Builder error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Error {
    /// Required field is not set.
    FieldNotSet(&'static str),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::FieldNotSet(field) => writeln!(f, "Required field is not set: `{}`", field),
        }
    }
}

impl error::Error for Error {}
