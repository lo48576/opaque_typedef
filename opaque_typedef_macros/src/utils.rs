//! Utilities.

use std::borrow::Cow;

use syn;

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
    #[allow(unknown_lints, option_option)]
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

pub fn extend_generics<'a, G>(
    generics: G,
    num_new_lifetimes: usize,
    new_where_predicates: &[syn::WherePredicate],
) -> (Cow<'a, syn::Generics>, Vec<syn::Lifetime>)
where
    G: Into<Cow<'a, syn::Generics>>,
{
    let mut generics_cow = generics.into();
    let mut new_lifetimes = Vec::with_capacity(num_new_lifetimes);
    if num_new_lifetimes != 0 {
        let generics = generics_cow.to_mut();
        for i in (0..num_new_lifetimes).rev() {
            let lt_str = format!("'__a{}", i);
            let lt =
                syn::parse_str::<syn::Lifetime>(&lt_str).expect("Failed to create lifetime tokens");
            new_lifetimes.push(lt.clone());
            generics
                .params
                .insert(0, syn::GenericParam::Lifetime(syn::LifetimeDef::new(lt)));
        }
    }
    if !new_where_predicates.is_empty() {
        let generics = generics_cow.to_mut();
        let where_clause = generics.make_where_clause();
        for pred in new_where_predicates {
            where_clause.predicates.push(pred.clone());
        }
    }
    (generics_cow, new_lifetimes)
}
