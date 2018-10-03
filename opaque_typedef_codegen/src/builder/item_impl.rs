//! [`ItemImpl`] builder.

use super::Error;

use syn;

/// [`syn::ItemImpl`] builder.
#[derive(Default, Debug, Clone)]
pub struct ItemImplBuilder {
    attrs: Vec<syn::Attribute>,
    generics: Option<syn::Generics>,
    trait_path: Option<syn::Path>,
    self_ty: Option<syn::Type>,
    impl_items: Vec<syn::ImplItem>,
}

impl ItemImplBuilder {
    /// Creates a new `ItemImplBuilder`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds the given attribute.
    pub fn attr(&mut self, attr: syn::Attribute) -> &mut Self {
        self.attrs.push(attr);
        self
    }

    /// Adds the given attributes.
    pub fn attrs<I>(&mut self, attrs: I) -> &mut Self
    where
        I: IntoIterator<Item = syn::Attribute>,
    {
        self.attrs.extend(attrs);
        self
    }

    /// Sets generics.
    pub fn generics(&mut self, generics: syn::Generics) -> &mut Self {
        self.generics = Some(generics);
        self
    }

    /// Sets the trait path.
    pub fn trait_path(&mut self, path: syn::Path) -> &mut Self {
        self.trait_path = Some(path);
        self
    }

    /// Sets the self type.
    pub fn self_ty(&mut self, self_ty: syn::Type) -> &mut Self {
        self.self_ty = Some(self_ty);
        self
    }

    /// Adds the given impl item.
    pub fn impl_item(&mut self, impl_item: syn::ImplItem) -> &mut Self {
        self.impl_items.push(impl_item);
        self
    }

    /// Adds the given impl items.
    pub fn impl_items<I>(&mut self, impl_items: I) -> &mut Self
    where
        I: IntoIterator<Item = syn::ImplItem>,
    {
        self.impl_items.extend(impl_items);
        self
    }

    /// Builds the [`syn::ItemImpl`].
    pub fn build(self) -> Result<syn::ItemImpl, Error> {
        let trait_ = Some((
            None,
            self.trait_path.ok_or(Error::FieldNotSet("trait_path"))?,
            <Token![for]>::default(),
        ));
        let self_ty = Box::new(self.self_ty.ok_or(Error::FieldNotSet("self_ty"))?);
        Ok(syn::ItemImpl {
            attrs: self.attrs,
            defaultness: None,
            unsafety: None,
            impl_token: Default::default(),
            generics: self.generics.unwrap_or_default(),
            trait_,
            self_ty,
            brace_token: Default::default(),
            items: self.impl_items,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::super::GenericsBuilder;
    use super::*;
    use proc_macro2::TokenStream;
    use std;

    #[test]
    fn empty() {
        let impl_ = ItemImplBuilder::new().build();
        assert!(impl_.is_err());
    }

    fn preset_builder() -> ItemImplBuilder {
        let mut builder = ItemImplBuilder::new();
        builder.self_ty(syn::parse2(quote! { SelfTy }).expect("Should never fail"));
        builder.trait_path(syn::parse2(quote! { path::Trait }).expect("Should never fail"));
        builder
    }

    fn attribute(path: TokenStream, tts: TokenStream) -> syn::Attribute {
        syn::Attribute {
            pound_token: Default::default(),
            style: syn::AttrStyle::Outer,
            bracket_token: Default::default(),
            path: syn::parse2(path).expect("Should never fail"),
            tts: syn::parse2(tts).expect("Should never fail"),
        }
    }

    #[test]
    fn preset() -> Result<(), Box<dyn std::error::Error>> {
        let impl_ = preset_builder().build()?;
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { impl path::Trait for SelfTy {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn attr() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.attr(attribute(quote! { hello }, quote! { (world) }));
        let impl_ = builder.build()?;
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { #[hello(world)] impl path::Trait for SelfTy {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn attrs() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.attrs(vec![
            attribute(quote! { hello }, quote! { (world) }),
            attribute(quote! { hello2 }, quote! { = world2 }),
        ]);
        let impl_ = builder.build()?;
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { #[hello(world)] #[hello2 = world2] impl path::Trait for SelfTy {} }
                .to_string(),
        );

        Ok(())
    }

    #[test]
    fn generics() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        let generics = {
            let mut builder = GenericsBuilder::new();
            builder.param(syn::parse2(quote! { T: Into<String> })?);
            builder.build().expect("Should return `Some`")
        };
        builder.generics(generics);
        let impl_ = builder.build()?;
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { impl<T: Into<String> > path::Trait for SelfTy {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn trait_path() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.trait_path(syn::parse2(quote! { path::to::the::Trait })?);
        let impl_ = builder.build()?;
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { impl path::to::the::Trait for SelfTy {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn self_ty() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.self_ty(syn::parse2(quote! { ThisIsSelfTy })?);
        let impl_ = builder.build()?;
        assert_eq!(
            quote! { #impl_ }.to_string(),
            quote! { impl path::Trait for ThisIsSelfTy {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn impl_item() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        let method = quote! {
            fn method(&self) -> Self {
                unimplemented!()
            }
        };
        builder.impl_item(syn::parse2(method.clone())?);
        let impl_ = builder.build()?;
        let expected = quote! {
            impl path::Trait for SelfTy {
                #method
            }
        };
        assert_eq!(quote! { #impl_ }.to_string(), expected.to_string(),);

        Ok(())
    }

    #[test]
    fn impl_items() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        let assoc_ty = quote! {
            type IntoIter = <Self as IntoIterator>::IntoIter;
        };
        let method = quote! {
            fn method(&self) -> Self {
                unimplemented!()
            }
        };
        builder.impl_items(vec![
            syn::parse2(assoc_ty.clone())?,
            syn::parse2(method.clone())?,
        ]);
        let impl_ = builder.build()?;
        let expected = quote! {
            impl path::Trait for SelfTy {
                #assoc_ty
                #method
            }
        };
        assert_eq!(quote! { #impl_ }.to_string(), expected.to_string(),);

        Ok(())
    }
}
