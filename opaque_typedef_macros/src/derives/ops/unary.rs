//! Impl generators for general unary operator traits.

use std::borrow::Cow;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn;

use type_props::TypeProps;
use utils::extend_generics;

use super::{OperandSpec, OperandTypeSpec, OperandTypeWrapperSpec};

/// Unary operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, EnumProperty)]
pub enum UnaryOpSpec {
    /// `std::ops::Neg`.
    #[strum(props(
        trait_ = "::std::ops::Neg",
        method = "neg",
        self_ = "self",
        ty_ret = "Self::Output"
    ))]
    Neg,
    /// `std::ops::Not`.
    #[strum(props(
        trait_ = "::std::ops::Not",
        method = "not",
        self_ = "self",
        ty_ret = "Self::Output"
    ))]
    Not,
}

impl UnaryOpSpec {
    fn parse_prop<T: syn::parse::Parse>(self, prop_name: &str) -> T {
        use strum::EnumProperty;

        let val = self.get_str(prop_name).unwrap_or_else(|| {
            panic!(
                "`UnaryOpSpec::{:?}` should have property `{}` but not found",
                self, prop_name
            );
        });
        syn::parse_str::<T>(val).unwrap_or_else(|e| {
            panic!(
                "`UnaryOpSpec::{:?}` has property `{} = {:?}`, but failed to parse: {}",
                self, prop_name, val, e
            );
        })
    }

    /// Returns target trait path.
    pub fn tokens_trait_path(self) -> TokenStream {
        self.parse_prop::<syn::Path>("trait_").into_token_stream()
    }

    /// Returns method name to implement.
    pub fn tokens_method(self) -> TokenStream {
        self.parse_prop::<syn::Ident>("method").into_token_stream()
    }

    pub fn tokens_arg_self(self) -> TokenStream {
        self.parse_prop::<syn::Expr>("self_").into_token_stream()
    }

    pub fn tokens_associated_ty_output<T: ToTokens>(self, ty_outer: T) -> Option<TokenStream> {
        match self {
            UnaryOpSpec::Neg | UnaryOpSpec::Not => Some(quote!(#ty_outer)),
        }
    }

    pub fn tokens_associated_stuff<T: ToTokens>(self, ty_outer: T) -> TokenStream {
        match self.tokens_associated_ty_output(&ty_outer) {
            Some(ty_output) => quote! {
                type Output = #ty_output;
            },
            None => quote!(),
        }
    }

    pub fn tokens_ty_ret(self) -> TokenStream {
        self.parse_prop::<syn::Type>("ty_ret").into_token_stream()
    }

    pub fn tokens_from_inner_result<T, U>(self, ty_outer: T, helper_trait: U) -> TokenStream
    where
        T: ToTokens,
        U: ToTokens,
    {
        match self {
            UnaryOpSpec::Neg | UnaryOpSpec::Not => quote!(<#ty_outer as #helper_trait>::from_inner),
        }
    }

    pub fn tokens_lhs_inner_arg(self, props: &TypeProps, lhs_spec: OperandSpec) -> TokenStream {
        let expr = quote!(self);
        match self {
            UnaryOpSpec::Neg | UnaryOpSpec::Not => match (lhs_spec.type_, lhs_spec.wrapper) {
                (OperandTypeSpec::Inner, _) => expr,
                (OperandTypeSpec::Outer, OperandTypeWrapperSpec::Raw) => {
                    props.tokens_outer_expr_into_inner(expr)
                }
                (OperandTypeSpec::Outer, OperandTypeWrapperSpec::Ref) => {
                    props.tokens_outer_expr_as_inner(expr)
                }
            },
        }
    }
}

pub fn gen_impl_sized_raw(
    props: &TypeProps,
    op_spec: UnaryOpSpec,
    lhs_spec: OperandTypeSpec,
) -> TokenStream {
    assert!(lhs_spec != OperandTypeSpec::Inner);
    gen_impl_sized(
        props,
        op_spec,
        lhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
    )
}

pub fn gen_impl_sized_ref(
    props: &TypeProps,
    op_spec: UnaryOpSpec,
    lhs_spec: OperandTypeSpec,
) -> TokenStream {
    assert!(lhs_spec != OperandTypeSpec::Inner);
    let gen_ref = || {
        gen_impl_sized(
            props,
            op_spec,
            lhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
        )
    };
    match op_spec {
        UnaryOpSpec::Neg | UnaryOpSpec::Not => {
            let ref_ = gen_ref();
            quote! {
                #ref_
            }
        }
    }
}

pub fn gen_impl_sized(
    props: &TypeProps,
    op_spec: UnaryOpSpec,
    lhs_spec: OperandSpec,
) -> TokenStream {
    let ty_outer_generic = {
        let ty_outer = &props.ty_outer;
        let type_generics = &props.type_generics;
        quote!(#ty_outer #type_generics)
    };
    let ty_inner = props.field_inner.ty();
    let target_trait = op_spec.tokens_trait_path();

    let (generics, ty_lhs_impl, ty_lhs_inner) = {
        let num_extra_lts_lhs = lhs_spec.num_required_extra_lifetimes();
        let num_extra_lts = num_extra_lts_lhs;
        let generics = Cow::Borrowed(props.generics);
        let (generics, new_lts) = if num_extra_lts > 0 {
            extend_generics(generics, num_extra_lts, &[])
        } else {
            (generics, vec![])
        };
        let ty_lhs_impl =
            lhs_spec.tokens_ty_operand(&new_lts[..num_extra_lts_lhs], ty_inner, &ty_outer_generic);
        let ty_lhs_inner =
            lhs_spec.tokens_ty_operand_inner(&new_lts[..num_extra_lts_lhs], ty_inner);
        (generics, ty_lhs_impl, ty_lhs_inner)
    };
    let (generics, _) = {
        let extra_preds = if props.has_type_params() {
            let pred = syn::parse_str::<syn::WherePredicate>(&format!(
                "{}: {}<Output={}>",
                ty_lhs_inner,
                target_trait,
                ty_inner.into_token_stream()
            ))
            .expect("Failed to generate `WherePredicate`");
            vec![pred]
        } else {
            Vec::new()
        };
        extend_generics(generics, 0, &extra_preds)
    };
    let (impl_generics, _, where_clause) = generics.split_for_impl();

    let lhs_self_arg = op_spec.tokens_arg_self();
    let associated = op_spec.tokens_associated_stuff(&ty_outer_generic);
    let method_name = op_spec.tokens_method();
    let ty_ret = op_spec.tokens_ty_ret();
    let self_inner = op_spec.tokens_lhs_inner_arg(props, lhs_spec);
    let from_inner_result =
        op_spec.tokens_from_inner_result(&ty_outer_generic, props.helper_trait());

    quote! {
        impl #impl_generics #target_trait for #ty_lhs_impl
        #where_clause
        {
            #associated

            fn #method_name(#lhs_self_arg) -> #ty_ret {
                #from_inner_result(
                    #target_trait::#method_name(#self_inner)
                )
            }
        }
    }
}
