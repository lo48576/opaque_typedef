//! Code generators to implement `std::cmp::*` traits.

use std::borrow::Cow;
use quote;


lazy_static! {
    /// `self` token.
    static ref TOKEN_SELF: quote::Tokens = quote!(self);
    /// `other` token.
    static ref TOKEN_OTHER: quote::Tokens = quote!(other);
}


/// Parameters used to implement `std::cmp::*` traits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplParams<'a, F> {
    /// Outer type.
    pub ty_outer: &'a quote::Tokens,
    /// Inner type.
    pub ty_inner: &'a quote::Tokens,
    /// Converter function from the expression of outer type into expression of inner type.
    pub outer_as_inner: F,
    /// `PartialEq<Inner> for Inner` function.
    pub inner_partial_eq_fn: &'a quote::Tokens,
    /// `PartialOrd<Inner> for Inner` function.
    pub inner_partial_ord_fn: &'a quote::Tokens,
    /// Unused lifetimes in `ty_outer` and `ty_inner`.
    pub free_lifetimes: &'a [&'a str],
}

impl<'a, F> ImplParams<'a, F>
where
    F: Fn(&quote::Tokens) -> quote::Tokens,
{
    /// Returns the type of reference to the inner type.
    pub fn to_inner_ref(
        &self,
        expr: &quote::Tokens,
        type_meta: (TypeWrap, InnerOrOuter),
    ) -> quote::Tokens {
        let (wrap, in_or_out) = type_meta;
        // `&Inner` or `Outer`.
        let expr_ref = wrap.to_ref(expr);
        // `&Inner`.
        match in_or_out {
            InnerOrOuter::Inner => expr_ref.into_owned(),
            InnerOrOuter::Outer => (self.outer_as_inner)(&*expr_ref),
        }
    }

    /// Returns base type.
    pub fn base_type(&self, in_or_out: InnerOrOuter) -> &'a quote::Tokens {
        match in_or_out {
            InnerOrOuter::Inner => self.ty_inner,
            InnerOrOuter::Outer => self.ty_outer,
        }
    }

    /// Implements comparation operator for `LHS op RHS` and `RHS op LHS`.
    pub fn impl_symmetric(
        &self,
        cmp_target: CmpTarget,
        lhs_meta: (TypeWrap, InnerOrOuter),
        rhs_meta: (TypeWrap, InnerOrOuter),
    ) -> quote::Tokens {
        let inner_cmp_fn = match cmp_target {
            CmpTarget::PartialEq => self.inner_partial_eq_fn,
            CmpTarget::PartialOrd => self.inner_partial_ord_fn,
        };
        let ty_lhs_base = self.base_type(lhs_meta.1);
        let ty_rhs_base = self.base_type(rhs_meta.1);
        let mut iter_lifetimes = self.free_lifetimes.into_iter();
        let (ty_lhs_wrapped_unref, ty_rhs_wrapped_unref) = {
            let mut iter_lifetime_toks = iter_lifetimes.by_ref().map(lifetime_name_to_toks);
            let ty_lhs_wrapped_unref = lhs_meta
                .0
                .ty_wrapped_unref(ty_lhs_base, iter_lifetime_toks.by_ref());
            let ty_rhs_wrapped_unref = rhs_meta
                .0
                .ty_wrapped_unref(ty_rhs_base, iter_lifetime_toks.by_ref());
            (ty_lhs_wrapped_unref, ty_rhs_wrapped_unref)
        };
        let num_used_lifetimes = self.free_lifetimes.len() - iter_lifetimes.as_slice().len();
        let lifetimes = &self.free_lifetimes[..num_used_lifetimes];
        let lhs_to_inner = |expr| self.to_inner_ref(expr, (lhs_meta.0, lhs_meta.1));
        let rhs_to_inner = |expr| self.to_inner_ref(expr, (rhs_meta.0, rhs_meta.1));
        let impl0 = impl_single(
            cmp_target,
            inner_cmp_fn,
            &ty_lhs_wrapped_unref,
            &ty_rhs_wrapped_unref,
            &lhs_to_inner,
            &rhs_to_inner,
            lifetimes,
        );
        let impl1 = impl_single(
            cmp_target,
            inner_cmp_fn,
            &ty_rhs_wrapped_unref,
            &ty_lhs_wrapped_unref,
            &rhs_to_inner,
            &lhs_to_inner,
            lifetimes,
        );
        quote! {
            #impl0
            #impl1
        }
    }
}


/// A wrapping of a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeWrap {
    /// `&Target`.
    Ref,
    /// `&&Target`.
    RefRef,
    /// `&Cow<Target>`.
    CowRef,
}

impl TypeWrap {
    /// Converts the given expression of the wrapped type into reference type, and returns the
    /// converted expression.
    ///
    /// ```text
    /// # #[macro_use] extern quote;
    /// # fn main() {
    /// assert_eq!(TypeWrap::Ref.to_ref(quote!(v)), quote!(v));
    /// assert_eq!(TypeWrap::RefRef.to_ref(quote!(v)), quote!(*v));
    /// assert_eq!(TypeWrap::CowRef.to_ref(quote!(v)), quote!(&**v));
    /// # }
    /// ```
    pub fn to_ref<'a>(&self, expr: &'a quote::Tokens) -> Cow<'a, quote::Tokens> {
        match *self {
            TypeWrap::Ref => Cow::Borrowed(expr),
            TypeWrap::RefRef => Cow::Owned(quote!(*#expr)),
            TypeWrap::CowRef => Cow::Owned(quote!(&**#expr)),
        }
    }

    /// Converts the given base type into wrapped type without outermost reference.
    ///
    /// ```text
    /// # #[macro_use] extern quote;
    /// # fn main() {
    /// assert_eq!(
    ///     TypeWrap::Ref.ty_wrapped_unref(quote!(T), &["lt"]),
    ///     quote!(T)
    /// );
    /// assert_eq!(
    ///     TypeWrap::RefRef.ty_wrapped_unref(quote!(T), &["lt"]),
    ///     quote!(&'lt T)
    /// );
    /// assert_eq!(
    ///     TypeWrap::CowRef.ty_wrapped_unref(quote!(T), &["lt"]),
    ///     quote!(Cow<'lt, T>)
    /// );
    /// # }
    /// ```
    ///
    /// Note that each elements of `free_lifetimes` should contain leading `'`.
    ///
    /// Panics if sufficient lifetimes are given.
    pub fn ty_wrapped_unref<'a, I, T>(
        &self,
        ty_base: &'a quote::Tokens,
        free_lifetimes: I,
    ) -> Cow<'a, quote::Tokens>
    where
        I: IntoIterator<Item = T>,
        T: ::std::borrow::Borrow<quote::Tokens>,
    {
        match *self {
            TypeWrap::Ref => Cow::Borrowed(ty_base),
            TypeWrap::RefRef => {
                let lt = free_lifetimes
                    .into_iter()
                    .next()
                    .expect("Need more lifetimes for `TypeWrap::RefRef`");
                let lt = lt.borrow();
                Cow::Owned(quote! { &#lt #ty_base })
            },
            TypeWrap::CowRef => {
                let lt = free_lifetimes
                    .into_iter()
                    .next()
                    .expect("Need more lifetimes for `TypeWrap::CowRef`");
                let lt = lt.borrow();
                Cow::Owned(quote! { ::std::borrow::Cow<#lt, #ty_base> })
            },
        }
    }
}


/// Inner or outer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InnerOrOuter {
    /// Inner.
    Inner,
    /// Outer.
    Outer,
}


/// Compare target trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CmpTarget {
    /// `PartialEq`.
    PartialEq,
    /// `PartialOrd`.
    PartialOrd,
}

impl CmpTarget {
    /// Returns the target trait.
    fn cmp_trait(&self) -> &'static quote::Tokens {
        lazy_static! {
            static ref PARTIAL_EQ_TRAIT: quote::Tokens = quote!{ ::std::cmp::PartialEq };
            static ref PARTIAL_ORD_TRAIT: quote::Tokens = quote!{ ::std::cmp::PartialOrd };
        }
        match *self {
            CmpTarget::PartialEq => &*PARTIAL_EQ_TRAIT,
            CmpTarget::PartialOrd => &*PARTIAL_ORD_TRAIT,
        }
    }

    /// Returns the default function to implement.
    fn cmp_fn(&self) -> &'static quote::Tokens {
        lazy_static! {
            static ref PARTIAL_EQ_FN: quote::Tokens = quote!{ eq };
            static ref PARTIAL_ORD_FN: quote::Tokens = quote!{ partial_cmp };
        }
        match *self {
            CmpTarget::PartialEq => &*PARTIAL_EQ_FN,
            CmpTarget::PartialOrd => &*PARTIAL_ORD_FN,
        }
    }

    /// Returns the return type of the default function to implement.
    fn ty_cmp_fn_ret(&self) -> &'static quote::Tokens {
        lazy_static! {
            static ref PARTIAL_EQ_RET: quote::Tokens = quote!{ bool };
            static ref PARTIAL_ORD_RET: quote::Tokens = quote!{ ::std::option::Option<::std::cmp::Ordering> };
        }
        match *self {
            CmpTarget::PartialEq => &*PARTIAL_EQ_RET,
            CmpTarget::PartialOrd => &*PARTIAL_ORD_RET,
        }
    }
}


/// Implements the target trait.
pub fn impl_single<'l, 'r, Fl, Fr>(
    target: CmpTarget,
    inner_cmp_fn: &quote::Tokens,
    ty_lhs: &quote::Tokens,
    ty_rhs: &quote::Tokens,
    lhs_to_inner: Fl,
    rhs_to_inner: Fr,
    lifetimes: &[&str],
) -> quote::Tokens
where
    Fl: Fn(&'l quote::Tokens) -> quote::Tokens,
    Fr: Fn(&'r quote::Tokens) -> quote::Tokens,
{
    let cmp_trait = target.cmp_trait();
    let cmp_fn = target.cmp_fn();
    let ty_cmp_fn_ret = target.ty_cmp_fn_ret();
    let expr_self = lhs_to_inner(&*TOKEN_SELF);
    let expr_other = rhs_to_inner(&*TOKEN_OTHER);
    let lifetimes = lifetime_names_to_toks(lifetimes)
        .map(|lts| quote! { <#lts> })
        .unwrap_or_default();
    quote! {
        impl #lifetimes #cmp_trait<#ty_rhs> for #ty_lhs {
            fn #cmp_fn(&self, other: &#ty_rhs) -> #ty_cmp_fn_ret {
                #inner_cmp_fn(#expr_self, #expr_other)
            }
        }
    }
}


/// Converts the list of lifetime names into the comma-separated lifetimes.
///
/// ```text
/// assert_eq!(lifetime_names_to_toks(&[] as &[&str]).as_ref(), None);
/// assert_eq!(
///     lifetime_names_to_toks(&["foo", "bar", "baz"])
///         .as_ref()
///         .map(AsRef::as_ref),
///     Some("'foo , 'bar , 'baz")
/// );
/// ```
fn lifetime_names_to_toks<S, I>(names: I) -> Option<quote::Tokens>
where
    S: AsRef<str>,
    I: IntoIterator<Item = S>,
{
    let iter = names
        .into_iter()
        .map(|name| lifetime_name_to_toks(name.as_ref()));
    let mut toks = quote!{};
    toks.append_separated(iter, ",");
    // TODO: Use `Option::filter` (see <https://github.com/rust-lang/rust/issues/45860>).
    Some(toks).into_iter().find(|v| !v.as_ref().is_empty())
}


/// Converts the given lifetime name into the tokens.
///
/// ```text
/// assert_eq!(lifetime_name_to_toks("foo").as_ref(), "'foo");
/// ```
fn lifetime_name_to_toks<S: AsRef<str>>(name: S) -> quote::Tokens {
    let name = name.as_ref();
    let mut toks = quote!{};
    let mut tok_string = String::with_capacity(name.len() + 1);
    tok_string.push_str("'");
    tok_string.push_str(name);
    toks.append(tok_string);
    toks
}

#[cfg(tests)]
mod tests {
    use super::*;

    #[test]
    fn test_typewrap_to_ref() {
        assert_eq!(TypeWrap::Ref.to_ref(quote!(v)), quote!(v));
        assert_eq!(TypeWrap::RefRef.to_ref(quote!(v)), quote!(*v));
        assert_eq!(TypeWrap::CowRef.to_ref(quote!(v)), quote!(&**v));
    }

    #[test]
    fn test_typewrap_ty_wrapped_unref() {
        assert_eq!(
            TypeWrap::Ref.ty_wrapped_unref(quote!(T), &["lt"]),
            quote!(T)
        );
        assert_eq!(
            TypeWrap::RefRef.ty_wrapped_unref(quote!(T), &["lt"]),
            quote!(&'lt T)
        );
        assert_eq!(
            TypeWrap::CowRef.ty_wrapped_unref(quote!(T), &["lt"]),
            quote!(Cow<'lt, T>)
        );
    }

    #[test]
    fn test_lifetime_names_to_toks() {
        assert_eq!(lifetime_names_to_toks(&[] as &[&str]).as_ref(), None);
        assert_eq!(
            lifetime_names_to_toks(&["foo", "bar", "baz"])
                .as_ref()
                .map(AsRef::as_ref),
            Some("'foo , 'bar , 'baz")
        );
    }

    #[test]
    fn test_lifetime_name_to_toks() {
        assert_eq!(lifetime_name_to_toks("foo").as_ref(), "'foo");
    }
}
