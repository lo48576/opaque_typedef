//! Impl generators for general binary operator traits.

use std::borrow::Cow;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn;

use type_props::TypeProps;
use utils::extend_generics;

use super::{OperandSpec, OperandTypeSpec, OperandTypeWrapperSpec};


/// Binary operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, EnumProperty)]
pub enum BinOpSpec {
    /// `std::ops::Add`.
    #[strum(
        props(
            trait_ = "::std::ops::Add",
            method = "add",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    Add,
    /// `std::ops::AddAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::AddAssign",
            method = "add_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    AddAssign,
    /// `std::ops::BitAnd`.
    #[strum(
        props(
            trait_ = "::std::ops::BitAnd",
            method = "bitand",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    BitAnd,
    /// `std::ops::AddAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::BitAndAssign",
            method = "bitand_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    BitAndAssign,
    /// `std::ops::BitOr`.
    #[strum(
        props(
            trait_ = "::std::ops::BitOr",
            method = "bitor",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    BitOr,
    /// `std::ops::AddAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::BitOrAssign",
            method = "bitor_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    BitOrAssign,
    /// `std::ops::BitXor`.
    #[strum(
        props(
            trait_ = "::std::ops::BitXor",
            method = "bitxor",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    BitXor,
    /// `std::ops::AddAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::BitXorAssign",
            method = "bitxor_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    BitXorAssign,
    /// `std::ops::Div`.
    #[strum(
        props(
            trait_ = "::std::ops::Div",
            method = "div",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    Div,
    /// `std::ops::DivAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::DivAssign",
            method = "div_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    DivAssign,
    /// `std::ops::Mul`.
    #[strum(
        props(
            trait_ = "::std::ops::Mul",
            method = "mul",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    Mul,
    /// `std::ops::MulAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::MulAssign",
            method = "mul_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    MulAssign,
    /// `std::ops::Rem`.
    #[strum(
        props(
            trait_ = "::std::ops::Rem",
            method = "rem",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    Rem,
    /// `std::ops::RemAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::RemAssign",
            method = "rem_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    RemAssign,
    /// `std::ops::Shl`.
    #[strum(
        props(
            trait_ = "::std::ops::Shl",
            method = "shl",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    Shl,
    /// `std::ops::ShlAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::ShlAssign",
            method = "shl_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    ShlAssign,
    /// `std::ops::Shr`.
    #[strum(
        props(
            trait_ = "::std::ops::Shr",
            method = "shr",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    Shr,
    /// `std::ops::ShrAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::ShrAssign",
            method = "shr_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    ShrAssign,
    /// `std::ops::Sub`.
    #[strum(
        props(
            trait_ = "::std::ops::Sub",
            method = "sub",
            self_ = "self",
            ty_ret = "Self::Output"
        )
    )]
    Sub,
    /// `std::ops::SubAssign`.
    #[strum(
        props(
            trait_ = "::std::ops::SubAssign",
            method = "sub_assign",
            self_ = "&mut self",
            ty_ret = "()"
        )
    )]
    SubAssign,
}

impl BinOpSpec {
    fn parse_prop<T: syn::synom::Synom>(self, prop_name: &str) -> T {
        use strum::EnumProperty;

        let val = self.get_str(prop_name).unwrap_or_else(|| {
            panic!(
                "`BinOpSpec::{:?}` should have property `{}` but not found",
                self, prop_name
            );
        });
        syn::parse_str::<T>(val).unwrap_or_else(|e| {
            panic!(
                "`BinOpSpec::{:?}` has property `{} = {:?}`, but failed to parse: {}",
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

    pub fn tokens_ty_rhs_arg<T: ToTokens>(self, ty_rhs: T) -> TokenStream {
        match self {
            BinOpSpec::Add
            | BinOpSpec::AddAssign
            | BinOpSpec::BitAnd
            | BinOpSpec::BitAndAssign
            | BinOpSpec::BitOr
            | BinOpSpec::BitOrAssign
            | BinOpSpec::BitXor
            | BinOpSpec::BitXorAssign
            | BinOpSpec::Div
            | BinOpSpec::DivAssign
            | BinOpSpec::Mul
            | BinOpSpec::MulAssign
            | BinOpSpec::Rem
            | BinOpSpec::RemAssign
            | BinOpSpec::Shl
            | BinOpSpec::ShlAssign
            | BinOpSpec::Shr
            | BinOpSpec::ShrAssign
            | BinOpSpec::Sub
            | BinOpSpec::SubAssign => ty_rhs.into_token_stream(),
        }
    }

    pub fn tokens_associated_ty_output<T: ToTokens>(self, ty_outer: T) -> Option<TokenStream> {
        match self {
            BinOpSpec::Add
            | BinOpSpec::BitAnd
            | BinOpSpec::BitOr
            | BinOpSpec::BitXor
            | BinOpSpec::Div
            | BinOpSpec::Mul
            | BinOpSpec::Rem
            | BinOpSpec::Shl
            | BinOpSpec::Shr
            | BinOpSpec::Sub => Some(ty_outer.into_token_stream()),
            _ => None,
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

    // TODO: Make it configurable to choose inner implementation of `*Assign`.
    // In other words, users should be able to select `a = a + b;` style or
    // `a.inner += b.inner;` style, for each `*Assign` trait impls.
    // Note that this feature also requires trait bounds generation to be
    // configurable.
    pub fn tokens_from_inner_result<T, U>(self, ty_outer: T, helper_trait: U) -> TokenStream
    where
        T: ToTokens,
        U: ToTokens,
    {
        match self {
            BinOpSpec::Add
            | BinOpSpec::BitAnd
            | BinOpSpec::BitOr
            | BinOpSpec::BitXor
            | BinOpSpec::Div
            | BinOpSpec::Mul
            | BinOpSpec::Rem
            | BinOpSpec::Shl
            | BinOpSpec::Shr
            | BinOpSpec::Sub => quote!(<#ty_outer as #helper_trait>::from_inner),
            BinOpSpec::AddAssign
            | BinOpSpec::BitAndAssign
            | BinOpSpec::BitOrAssign
            | BinOpSpec::BitXorAssign
            | BinOpSpec::DivAssign
            | BinOpSpec::MulAssign
            | BinOpSpec::RemAssign
            | BinOpSpec::ShlAssign
            | BinOpSpec::ShrAssign
            | BinOpSpec::SubAssign => quote!(),
        }
    }

    pub fn tokens_lhs_inner_arg(self, props: &TypeProps, lhs_spec: OperandSpec) -> TokenStream {
        let expr = quote!(self);
        match self {
            BinOpSpec::Add
            | BinOpSpec::BitAnd
            | BinOpSpec::BitOr
            | BinOpSpec::BitXor
            | BinOpSpec::Div
            | BinOpSpec::Mul
            | BinOpSpec::Rem
            | BinOpSpec::Shl
            | BinOpSpec::Shr
            | BinOpSpec::Sub => match (lhs_spec.type_, lhs_spec.wrapper) {
                (OperandTypeSpec::Inner, _) => expr,
                (OperandTypeSpec::Outer, OperandTypeWrapperSpec::Raw) => {
                    props.tokens_outer_expr_into_inner(expr)
                },
                (OperandTypeSpec::Outer, OperandTypeWrapperSpec::Ref) => {
                    props.tokens_outer_expr_as_inner(expr)
                },
            },
            BinOpSpec::AddAssign
            | BinOpSpec::BitAndAssign
            | BinOpSpec::BitOrAssign
            | BinOpSpec::BitXorAssign
            | BinOpSpec::DivAssign
            | BinOpSpec::MulAssign
            | BinOpSpec::RemAssign
            | BinOpSpec::ShlAssign
            | BinOpSpec::ShrAssign
            | BinOpSpec::SubAssign => match (lhs_spec.type_, lhs_spec.wrapper) {
                (OperandTypeSpec::Inner, _) => quote!(&mut #expr),
                (OperandTypeSpec::Outer, _) => props.tokens_outer_expr_as_inner_mut_nocheck(expr),
            },
        }
    }

    pub fn tokens_rhs_inner_arg<T: ToTokens>(
        self,
        props: &TypeProps,
        rhs_spec: OperandSpec,
        rhs_expr: T,
    ) -> TokenStream {
        let inner = rhs_spec.tokens_inner(props, rhs_expr);
        match self {
            BinOpSpec::Add
            | BinOpSpec::BitAnd
            | BinOpSpec::BitOr
            | BinOpSpec::BitXor
            | BinOpSpec::Div
            | BinOpSpec::Mul
            | BinOpSpec::Rem
            | BinOpSpec::Shl
            | BinOpSpec::Shr
            | BinOpSpec::Sub
            | BinOpSpec::AddAssign
            | BinOpSpec::BitAndAssign
            | BinOpSpec::BitOrAssign
            | BinOpSpec::BitXorAssign
            | BinOpSpec::DivAssign
            | BinOpSpec::MulAssign
            | BinOpSpec::RemAssign
            | BinOpSpec::ShlAssign
            | BinOpSpec::ShrAssign
            | BinOpSpec::SubAssign => inner,
        }
    }
}


pub fn gen_impl_sized_raw(
    props: &TypeProps,
    op_spec: BinOpSpec,
    lhs_spec: OperandTypeSpec,
    rhs_spec: OperandTypeSpec,
) -> TokenStream {
    assert!(lhs_spec != OperandTypeSpec::Inner || rhs_spec != OperandTypeSpec::Inner);
    gen_impl_sized(
        props,
        op_spec,
        lhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
        rhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
    )
}


pub fn gen_impl_sized_ref(
    props: &TypeProps,
    op_spec: BinOpSpec,
    lhs_spec: OperandTypeSpec,
    rhs_spec: OperandTypeSpec,
) -> TokenStream {
    assert!(lhs_spec != OperandTypeSpec::Inner || rhs_spec != OperandTypeSpec::Inner);
    let gen_raw_ref = || {
        gen_impl_sized(
            props,
            op_spec,
            lhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
            rhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
        )
    };
    let gen_ref_raw = || {
        gen_impl_sized(
            props,
            op_spec,
            lhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
            rhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
        )
    };
    let gen_ref_ref = || {
        gen_impl_sized(
            props,
            op_spec,
            lhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
            rhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
        )
    };
    match op_spec {
        BinOpSpec::Add
        | BinOpSpec::BitAnd
        | BinOpSpec::BitOr
        | BinOpSpec::BitXor
        | BinOpSpec::Div
        | BinOpSpec::Mul
        | BinOpSpec::Rem
        | BinOpSpec::Shl
        | BinOpSpec::Shr
        | BinOpSpec::Sub => {
            let raw_ref = gen_raw_ref();
            let ref_raw = gen_ref_raw();
            let ref_ref = gen_ref_ref();
            quote! {
                #raw_ref
                #ref_raw
                #ref_ref
            }
        },
        BinOpSpec::AddAssign
        | BinOpSpec::BitAndAssign
        | BinOpSpec::BitOrAssign
        | BinOpSpec::BitXorAssign
        | BinOpSpec::DivAssign
        | BinOpSpec::MulAssign
        | BinOpSpec::RemAssign
        | BinOpSpec::ShlAssign
        | BinOpSpec::ShrAssign
        | BinOpSpec::SubAssign => {
            let raw_ref = gen_raw_ref();
            quote! {
                #raw_ref
            }
        },
    }
}


pub fn gen_impl_sized(
    props: &TypeProps,
    op_spec: BinOpSpec,
    lhs_spec: OperandSpec,
    rhs_spec: OperandSpec,
) -> TokenStream {
    let ty_outer_generic = {
        let ty_outer = &props.ty_outer;
        let type_generics = &props.type_generics;
        quote!(#ty_outer #type_generics)
    };
    let ty_inner = props.field_inner.ty();
    let target_trait = op_spec.tokens_trait_path();

    let (generics, ty_lhs_impl, ty_rhs_impl, ty_lhs_inner, ty_rhs_inner) = {
        let num_extra_lts_lhs = lhs_spec.num_required_extra_lifetimes();
        let num_extra_lts_rhs = rhs_spec.num_required_extra_lifetimes();
        let num_extra_lts = num_extra_lts_lhs + num_extra_lts_rhs;
        let generics = Cow::Borrowed(props.generics);
        let (generics, new_lts) = if num_extra_lts > 0 {
            extend_generics(generics, num_extra_lts, &[])
        } else {
            (generics, vec![])
        };
        let ty_lhs_impl =
            lhs_spec.tokens_ty_operand(&new_lts[..num_extra_lts_lhs], ty_inner, &ty_outer_generic);
        let ty_rhs_impl =
            rhs_spec.tokens_ty_operand(&new_lts[num_extra_lts_lhs..], ty_inner, &ty_outer_generic);
        let ty_lhs_inner =
            lhs_spec.tokens_ty_operand_inner(&new_lts[..num_extra_lts_lhs], ty_inner);
        let ty_rhs_inner =
            rhs_spec.tokens_ty_operand_inner(&new_lts[num_extra_lts_lhs..], ty_inner);
        (
            generics,
            ty_lhs_impl,
            ty_rhs_impl,
            ty_lhs_inner,
            ty_rhs_inner,
        )
    };

    let (generics, _) = {
        let extra_preds = if props.has_type_params() {
            let associated_ty_output = op_spec.tokens_associated_ty_output(&ty_outer_generic);
            let pred_str = match associated_ty_output {
                Some(_) => format!(
                    "{}: {}<{}, Output={}>",
                    ty_lhs_inner,
                    target_trait,
                    ty_rhs_inner,
                    ty_inner.into_token_stream()
                ),
                None => format!("{}: {}<{}>", ty_lhs_inner, target_trait, ty_rhs_inner),
            };
            let pred = syn::parse_str::<syn::WherePredicate>(&pred_str)
                .expect("Failed to generate `WherePredicate`");
            vec![pred]
        } else {
            Vec::new()
        };
        extend_generics(generics, 0, &extra_preds)
    };
    let (impl_generics, _, where_clause) = generics.split_for_impl();

    let other = quote!(other);
    let lhs_self_arg = op_spec.tokens_arg_self();
    let ty_rhs_arg = op_spec.tokens_ty_rhs_arg(&ty_rhs_impl);
    let associated = op_spec.tokens_associated_stuff(&ty_outer_generic);
    let method_name = op_spec.tokens_method();
    let ty_ret = op_spec.tokens_ty_ret();
    let self_inner = op_spec.tokens_lhs_inner_arg(props, lhs_spec);
    let other_inner = op_spec.tokens_rhs_inner_arg(props, rhs_spec, &other);
    let from_inner_result =
        op_spec.tokens_from_inner_result(&ty_outer_generic, props.helper_trait());

    quote! {
        impl #impl_generics #target_trait<#ty_rhs_impl> for #ty_lhs_impl
        #where_clause
        {
            #associated

            fn #method_name(#lhs_self_arg, #other: #ty_rhs_arg) -> #ty_ret {
                #from_inner_result(
                    #target_trait::#method_name(
                        #self_inner,
                        #other_inner
                    )
                )
            }
        }
    }
}
