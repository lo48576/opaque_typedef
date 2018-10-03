//! [`syn::ImplItemMethod`] builder.

use super::Error;

use syn;

/// [`syn::ImplItemMethod`] builder.
#[derive(Default, Debug, Clone)]
pub struct ImplItemMethodBuilder {
    attrs: Vec<syn::Attribute>,
    ident: Option<syn::Ident>,
    generics: Option<syn::Generics>,
    inputs: Vec<syn::FnArg>,
    output: Option<syn::ReturnType>,
    block: Option<syn::Block>,
}

impl ImplItemMethodBuilder {
    /// Creates a new `ImplItemMethodBuilder`.
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

    /// Sets ident.
    pub fn ident(&mut self, ident: syn::Ident) -> &mut Self {
        self.ident = Some(ident);
        self
    }

    /// Sets generics.
    pub fn generics(&mut self, generics: syn::Generics) -> &mut Self {
        self.generics = Some(generics);
        self
    }

    /// Adds the given argument.
    pub fn arg(&mut self, arg: syn::FnArg) -> &mut Self {
        self.inputs.push(arg);
        self
    }

    /// Adds the given arguments.
    pub fn args<I>(&mut self, args: I) -> &mut Self
    where
        I: IntoIterator<Item = syn::FnArg>,
    {
        self.inputs.extend(args);
        self
    }

    /// Sets return type.
    pub fn output(&mut self, output: syn::ReturnType) -> &mut Self {
        self.output = Some(output);
        self
    }

    /// Sets the block.
    pub fn block(&mut self, block: syn::Block) -> &mut Self {
        self.block = Some(block);
        self
    }

    /// Builds the [`syn::ImplItemMethod`].
    pub fn build(self) -> Result<syn::ImplItemMethod, Error> {
        let decl = syn::FnDecl {
            fn_token: Default::default(),
            generics: self.generics.unwrap_or_default(),
            paren_token: Default::default(),
            inputs: self.inputs.into_iter().collect(),
            variadic: None,
            output: self.output.ok_or(Error::FieldNotSet("output"))?,
        };
        let sig = syn::MethodSig {
            constness: None,
            unsafety: None,
            asyncness: None,
            abi: None,
            ident: self.ident.ok_or(Error::FieldNotSet("ident"))?,
            decl,
        };

        Ok(syn::ImplItemMethod {
            attrs: self.attrs,
            vis: syn::Visibility::Inherited,
            defaultness: None,
            sig,
            block: self.block.ok_or(Error::FieldNotSet("block"))?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::super::GenericsBuilder;
    use super::*;
    use proc_macro2::{Span, TokenStream};
    use std;

    #[test]
    fn empty() {
        let method = ImplItemMethodBuilder::new().build();
        assert!(method.is_err());
    }

    fn preset_builder() -> ImplItemMethodBuilder {
        let mut builder = ImplItemMethodBuilder::new();
        builder.ident(syn::Ident::new("ident", Span::call_site()));
        builder.output(syn::ReturnType::Default);
        builder.block(syn::parse2(quote! { {} }).expect("Should never fail"));
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
        let method = preset_builder().build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { fn ident() {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn ident() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.ident(syn::Ident::new("hello", Span::call_site()));
        let method = builder.build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { fn hello() {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn attr() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.attr(attribute(quote! { hello }, quote! { (world) }));
        let method = builder.build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { #[hello(world)] fn ident() {} }.to_string(),
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
        let method = builder.build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { #[hello(world)] #[hello2 = world2] fn ident() {} }.to_string(),
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
        let method = builder.build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { fn ident<T: Into<String> >() {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn arg() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.arg(syn::parse2(quote! { _: i32 })?);
        let method = builder.build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { fn ident(_: i32) {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn args() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.args(vec![
            syn::parse2(quote! { _foo: i32 })?,
            syn::parse2(quote! { (bar, baz): ((), !) })?,
        ]);
        let method = builder.build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { fn ident(_foo: i32, (bar, baz): ((), !)) {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn output() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.output(syn::parse2(quote! { -> &str })?);
        let method = builder.build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { fn ident() -> &str {} }.to_string(),
        );

        Ok(())
    }

    #[test]
    fn block() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = preset_builder();
        builder.block(syn::parse2(quote! { { println!("hello"); } })?);
        let method = builder.build()?;
        assert_eq!(
            quote! { #method }.to_string(),
            quote! { fn ident() { println!("hello"); } }.to_string(),
        );

        Ok(())
    }
}
