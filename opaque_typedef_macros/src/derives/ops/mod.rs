//! Impl generators for general unary / binary operator traits.

use quote;
use quote::ToTokens;
use syn;

use type_props::TypeProps;

pub mod binary;


/// Operand type (inner or outer).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OperandTypeSpec {
    /// Outer type or its variant.
    Outer,
    /// Inner type or its variant.
    Inner,
}

impl OperandTypeSpec {
    pub fn with_wrapper(self, w: OperandTypeWrapperSpec) -> OperandSpec {
        OperandSpec::new(self, w)
    }
}


/// Operand type wrapper (raw, ref, `Cow`, etc...).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OperandTypeWrapperSpec {
    /// Raw type.
    Raw,
    /// Reference.
    Ref,
}


/// Operand type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OperandSpec {
    pub type_: OperandTypeSpec,
    pub wrapper: OperandTypeWrapperSpec,
}

impl OperandSpec {
    /// Creates a new `OperandTypeSpec`.
    pub fn new(type_: OperandTypeSpec, wrapper: OperandTypeWrapperSpec) -> Self {
        Self { type_, wrapper }
    }

    /// Returns expression converted into the inner type.
    pub fn tokens_inner<T: ToTokens>(&self, props: &TypeProps, expr: T) -> quote::Tokens {
        match (self.type_, self.wrapper) {
            (OperandTypeSpec::Inner, OperandTypeWrapperSpec::Raw) => expr.into_tokens(),
            (OperandTypeSpec::Outer, OperandTypeWrapperSpec::Raw) => {
                props.tokens_outer_expr_into_inner(expr)
            },
            (OperandTypeSpec::Inner, OperandTypeWrapperSpec::Ref) => expr.into_tokens(),
            (OperandTypeSpec::Outer, OperandTypeWrapperSpec::Ref) => {
                props.tokens_outer_expr_as_inner(expr)
            },
        }
    }

    /// Returns operand type.
    pub fn tokens_ty_operand<T, U>(
        &self,
        extra_lt: &[syn::Lifetime],
        ty_inner: T,
        ty_outer: U,
    ) -> quote::Tokens
    where
        T: ToTokens,
        U: ToTokens,
    {
        assert!(extra_lt.len() >= self.num_required_extra_lifetimes());
        match self.wrapper {
            OperandTypeWrapperSpec::Raw => match self.type_ {
                OperandTypeSpec::Outer => ty_outer.into_tokens(),
                OperandTypeSpec::Inner => ty_inner.into_tokens(),
            },
            OperandTypeWrapperSpec::Ref => {
                let lt = extra_lt[0];
                match self.type_ {
                    OperandTypeSpec::Outer => quote!(&#lt #ty_outer),
                    OperandTypeSpec::Inner => quote!(&#lt #ty_inner),
                }
            },
        }
    }

    /// Returns inner type to be propageted.
    pub fn tokens_ty_operand_inner<T>(
        &self,
        extra_lt: &[syn::Lifetime],
        ty_inner: T,
    ) -> quote::Tokens
    where
        T: ToTokens,
    {
        assert!(extra_lt.len() >= self.num_required_extra_lifetimes());
        match self.wrapper {
            OperandTypeWrapperSpec::Raw => ty_inner.into_tokens(),
            OperandTypeWrapperSpec::Ref => {
                let lt = extra_lt[0];
                quote!(&#lt #ty_inner)
            },
        }
    }

    pub fn num_required_extra_lifetimes(&self) -> usize {
        match self.wrapper {
            OperandTypeWrapperSpec::Raw => 0,
            OperandTypeWrapperSpec::Ref => 1,
        }
    }
}
