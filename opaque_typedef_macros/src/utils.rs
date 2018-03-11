//! Utilities.


/// Result of `expect_singleton_iter()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SingletonIterResult<T> {
    /// Iterator emits no items.
    None,
    /// Iterator emits a single item.
    Single(T),
    /// Iterator emits multiple items.
    ///
    /// This variant is `Multiple(first, second)`.
    Multiple(T, T),
}

impl<T> SingletonIterResult<T> {
    /// Returns `Some(None)` for no items, `Some(Some(T))` for a single item,
    /// `None` for multiple items.
    pub fn at_most_one(self) -> Option<Option<T>> {
        match self {
            SingletonIterResult::None => Some(None),
            SingletonIterResult::Single(v) => Some(Some(v)),
            SingletonIterResult::Multiple(..) => None,
        }
    }
}


/// Checks whether the iterator has at most one item, and returns the items.
pub fn expect_singleton_iter<I, T>(iter: I) -> SingletonIterResult<T>
where
    I: IntoIterator<Item = T>,
{
    let mut iter = iter.into_iter();
    let first = match iter.next() {
        Some(v) => v,
        None => return SingletonIterResult::None,
    };
    match iter.next() {
        Some(second) => SingletonIterResult::Multiple(first, second),
        None => SingletonIterResult::Single(first),
    }
}
