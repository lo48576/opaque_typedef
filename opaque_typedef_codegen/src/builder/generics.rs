//! [`syn::Generics`] builder.

use syn;

/// [`syn::Generics`] builder.
#[derive(Default, Debug, Clone)]
pub struct GenericsBuilder {
    params: Vec<syn::GenericParam>,
    where_preds: Vec<syn::WherePredicate>,
}

impl GenericsBuilder {
    /// Creates a new `GenericsBuilder`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds the given generic parameter.
    pub fn param(&mut self, param: syn::GenericParam) -> &mut Self {
        self.params.push(param);
        self
    }

    /// Adds the given generic parameters.
    pub fn params<I>(&mut self, params: I) -> &mut Self
    where
        I: IntoIterator<Item = syn::GenericParam>,
    {
        self.params.extend(params);
        self
    }

    /// Adds the given where predicate.
    pub fn where_pred(&mut self, pred: syn::WherePredicate) -> &mut Self {
        self.where_preds.push(pred);
        self
    }

    /// Adds the given where predicates.
    pub fn where_preds<I>(&mut self, preds: I) -> &mut Self
    where
        I: IntoIterator<Item = syn::WherePredicate>,
    {
        self.where_preds.extend(preds);
        self
    }

    /// Builds the [`Option<syn::Generics>`][`syn::Generics`].
    pub fn build(self) -> Option<syn::Generics> {
        if self.params.is_empty() {
            return None;
        }

        let where_clause = if self.where_preds.is_empty() {
            None
        } else {
            Some(syn::WhereClause {
                where_token: <Token![where]>::default(),
                predicates: self.where_preds.into_iter().collect(),
            })
        };
        Some(syn::Generics {
            lt_token: Some(<Token![<]>::default()),
            params: self.params.into_iter().collect(),
            gt_token: Some(<Token![>]>::default()),
            where_clause,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std;

    #[test]
    fn empty() {
        let generics = GenericsBuilder::new().build();
        assert!(generics.is_none());
    }

    #[test]
    fn param() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = GenericsBuilder::new();
        builder.param(syn::parse2(quote! { T: Into<String> })?);
        let generics = builder.build().expect("Should return `Some`");
        let (impl_, ty_, where_) = generics.split_for_impl();
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { <T: Into<String> > }.to_string()
        );
        assert_eq!(quote! { #ty_ }.to_string(), quote! { <T> }.to_string());
        assert!(where_.is_none());

        Ok(())
    }

    #[test]
    fn params() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = GenericsBuilder::new();
        builder.params(vec![
            syn::parse2(quote! { 'a })?,
            syn::parse2(quote! { T: Into<String> })?,
            syn::parse2(quote! { U: 'a + AsRef<str> })?,
        ]);
        let generics = builder.build().expect("Should return `Some`");
        let (impl_, ty_, where_) = generics.split_for_impl();
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { <'a, T: Into<String>, U: 'a + AsRef<str> > }.to_string()
        );
        assert_eq!(
            quote! { #ty_ }.to_string(),
            quote! { <'a, T, U> }.to_string()
        );
        assert!(where_.is_none());

        Ok(())
    }

    #[test]
    fn where_pred() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = GenericsBuilder::new();
        builder.param(syn::parse2(quote! { T })?);
        builder.where_pred(syn::parse2(quote! { T: Into<String> })?);
        let generics = builder.build().expect("Should return `Some`");
        let (impl_, ty_, where_) = generics.split_for_impl();
        assert_eq!(quote! { #impl_ }.to_string(), quote! { <T> }.to_string());
        assert_eq!(quote! { #ty_ }.to_string(), quote! { <T> }.to_string());
        assert_eq!(
            quote! { #where_ }.to_string(),
            quote! { where T: Into<String> }.to_string()
        );

        Ok(())
    }

    #[test]
    fn where_perds() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = GenericsBuilder::new();
        builder.params(vec![
            syn::parse2(quote! { 'a })?,
            syn::parse2(quote! { T })?,
            syn::parse2(quote! { U })?,
        ]);
        builder.where_preds(vec![
            syn::parse2(quote! { T: Into<String> })?,
            syn::parse2(quote! { U: 'a + AsRef<str> })?,
        ]);
        let generics = builder.build().expect("Should return `Some`");
        let (impl_, ty_, where_) = generics.split_for_impl();
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { <'a, T, U> }.to_string()
        );
        assert_eq!(
            quote! { #ty_ }.to_string(),
            quote! { <'a, T, U> }.to_string()
        );
        assert_eq!(
            quote! { #where_ }.to_string(),
            quote! { where T: Into<String>, U: 'a + AsRef<str> }.to_string()
        );

        Ok(())
    }
}
