//! Impl generators for general unary / binary operator traits.

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn;

use derives::Derive;
use type_props::TypeProps;

pub mod binary;
pub mod unary;


/// Operand type (inner or outer).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString)]
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
    pub fn tokens_inner<T: ToTokens>(self, props: &TypeProps, expr: T) -> TokenStream {
        match (self.type_, self.wrapper) {
            (OperandTypeSpec::Inner, OperandTypeWrapperSpec::Raw) => expr.into_token_stream(),
            (OperandTypeSpec::Outer, OperandTypeWrapperSpec::Raw) => {
                props.tokens_outer_expr_into_inner(expr)
            },
            (OperandTypeSpec::Inner, OperandTypeWrapperSpec::Ref) => expr.into_token_stream(),
            (OperandTypeSpec::Outer, OperandTypeWrapperSpec::Ref) => {
                props.tokens_outer_expr_as_inner(expr)
            },
        }
    }

    /// Returns operand type.
    pub fn tokens_ty_operand<T, U>(
        self,
        extra_lt: &[syn::Lifetime],
        ty_inner: T,
        ty_outer: U,
    ) -> TokenStream
    where
        T: ToTokens,
        U: ToTokens,
    {
        assert!(extra_lt.len() >= self.num_required_extra_lifetimes());
        match self.wrapper {
            OperandTypeWrapperSpec::Raw => match self.type_ {
                OperandTypeSpec::Outer => ty_outer.into_token_stream(),
                OperandTypeSpec::Inner => ty_inner.into_token_stream(),
            },
            OperandTypeWrapperSpec::Ref => {
                let lt = &extra_lt[0];
                match self.type_ {
                    OperandTypeSpec::Outer => quote!(&#lt #ty_outer),
                    OperandTypeSpec::Inner => quote!(&#lt #ty_inner),
                }
            },
        }
    }

    /// Returns inner type to be propageted.
    pub fn tokens_ty_operand_inner<T>(self, extra_lt: &[syn::Lifetime], ty_inner: T) -> TokenStream
    where
        T: ToTokens,
    {
        assert!(extra_lt.len() >= self.num_required_extra_lifetimes());
        match self.wrapper {
            OperandTypeWrapperSpec::Raw => ty_inner.into_token_stream(),
            OperandTypeWrapperSpec::Ref => {
                let lt = &extra_lt[0];
                quote!(&#lt #ty_inner)
            },
        }
    }

    pub fn num_required_extra_lifetimes(self) -> usize {
        match self.wrapper {
            OperandTypeWrapperSpec::Raw => 0,
            OperandTypeWrapperSpec::Ref => 1,
        }
    }
}


/// Operator impl variation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString)]
pub enum OpImplVariation {
    /// Use target (inner / outer) type directly.
    Direct,
    /// Use target (inner / outer) type and their references.
    References,
}


/// Operator spec.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpSpec {
    /// Unary operator.
    Unary {
        op_spec: unary::UnaryOpSpec,
        variation: OpImplVariation,
        lhs: OperandTypeSpec,
    },
    /// Binary operator.
    Binary {
        op_spec: binary::BinOpSpec,
        variation: OpImplVariation,
        lhs: OperandTypeSpec,
        rhs: OperandTypeSpec,
    },
}

impl OpSpec {
    /// Creates `OpSpec` from a derive target.
    pub fn from_derive_target(target: Derive) -> Option<Self> {
        //use std::str::FromStr;
        use strum::EnumProperty;

        let op = target.get_str("op")?;
        if let Ok(op_spec) = op.parse::<unary::UnaryOpSpec>() {
            let lhs = target.get_str("lhs").unwrap_or_else(|| {
                panic!(
                    "Unary operator {:?} should have `lhs` property but not",
                    op_spec
                );
            });
            let lhs: OperandTypeSpec = lhs.parse().unwrap_or_else(|_| {
                panic!(
                    "Operator property `lhs` = {:?} for unary operator {:?} is invalid",
                    lhs, op_spec
                );
            });
            let variation = target.get_str("variation").unwrap_or_else(|| {
                panic!(
                    "Unary operator {:?} should have `variation` property but not",
                    op_spec
                );
            });
            let variation: OpImplVariation = variation.parse().unwrap_or_else(|_| {
                panic!(
                    "Operator property `variation` = {:?} for unary operator {:?} is invalid",
                    variation, op_spec
                );
            });
            Some(OpSpec::Unary {
                op_spec,
                variation,
                lhs,
            })
        } else if let Ok(op_spec) = op.parse::<binary::BinOpSpec>() {
            let lhs = target.get_str("lhs").unwrap_or_else(|| {
                panic!(
                    "Binary operator {:?} should have `lhs` property but not",
                    op_spec
                );
            });
            let lhs: OperandTypeSpec = lhs.parse().unwrap_or_else(|_| {
                panic!(
                    "Operator property `lhs` = {:?} for binary operator {:?} is invalid",
                    lhs, op_spec
                );
            });
            let rhs = target.get_str("rhs").unwrap_or_else(|| {
                panic!(
                    "Binary operator {:?} should have `rhs` property but not",
                    op_spec
                );
            });
            let rhs: OperandTypeSpec = rhs.parse().unwrap_or_else(|_| {
                panic!(
                    "Operator property `rhs` = {:?} for binary operator {:?} is invalid",
                    rhs, op_spec
                );
            });
            let variation = target.get_str("variation").unwrap_or_else(|| {
                panic!(
                    "Binary operator {:?} should have `variation` property but not",
                    op_spec
                );
            });
            let variation: OpImplVariation = variation.parse().unwrap_or_else(|_| {
                panic!(
                    "Operator property `variation` = {:?} for binary operator {:?} is invalid",
                    variation, op_spec
                );
            });
            Some(OpSpec::Binary {
                op_spec,
                variation,
                lhs,
                rhs,
            })
        } else {
            panic!(
                "Derive target {:?} has invalid property op={:?}",
                target, op
            )
        }
    }

    pub fn gen_impl_sized(self, props: &TypeProps, _target: Derive) -> TokenStream {
        match self {
            OpSpec::Unary {
                op_spec,
                variation: OpImplVariation::Direct,
                lhs,
            } => unary::gen_impl_sized_raw(props, op_spec, lhs),
            OpSpec::Unary {
                op_spec,
                variation: OpImplVariation::References,
                lhs,
            } => unary::gen_impl_sized_ref(props, op_spec, lhs),
            OpSpec::Binary {
                op_spec,
                variation: OpImplVariation::Direct,
                lhs,
                rhs,
            } => binary::gen_impl_sized_raw(props, op_spec, lhs, rhs),
            OpSpec::Binary {
                op_spec,
                variation: OpImplVariation::References,
                lhs,
                rhs,
            } => binary::gen_impl_sized_ref(props, op_spec, lhs, rhs),
        }
    }

    pub fn gen_impl_unsized(self, _props: &TypeProps, target: Derive) -> TokenStream {
        panic!(
            "`#[opaque_typedef(derive({:?}))]` is currently not supported for unsized types",
            target
        );
    }
}
