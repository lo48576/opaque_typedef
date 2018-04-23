//! Impl generators for general unary / binary operator traits.

use std::borrow::Cow;

use quote;
use quote::ToTokens;
use syn;

use type_props::TypeProps;
use utils::extend_generics;


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


/// Binary operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOpSpec {
    /// `std::ops::Add`.
    Add,
}

impl BinOpSpec {
    /// Returns target trait path.
    pub fn tokens_trait_path(&self) -> quote::Tokens {
        match *self {
            BinOpSpec::Add => quote!(::std::ops::Add),
        }
    }

    /// Returns method name to implement.
    pub fn tokens_method(&self) -> quote::Tokens {
        match *self {
            BinOpSpec::Add => quote!(add),
        }
    }

    pub fn tokens_arg_self(&self) -> quote::Tokens {
        match *self {
            BinOpSpec::Add => quote!(self),
        }
    }

    pub fn tokens_ty_rhs_arg<T: ToTokens>(&self, ty_rhs: T) -> quote::Tokens {
        match *self {
            BinOpSpec::Add => ty_rhs.into_tokens(),
        }
    }

    pub fn tokens_associated_stuff<T: ToTokens>(&self, ty_outer: T) -> quote::Tokens {
        match *self {
            BinOpSpec::Add => quote! {
                type Output = #ty_outer;
            },
        }
    }

    pub fn tokens_ty_ret(&self) -> quote::Tokens {
        match *self {
            BinOpSpec::Add => quote!(Self::Output),
        }
    }

    pub fn tokens_body<T, U, V>(
        &self,
        ty_outer: T,
        helper_trait: U,
        inner_result: V,
    ) -> quote::Tokens
    where
        T: ToTokens,
        U: ToTokens,
        V: ToTokens,
    {
        match *self {
            BinOpSpec::Add => quote!(<#ty_outer as #helper_trait>::from_inner(#inner_result)),
        }
    }
}


pub fn gen_impl_bin_op_sized_raw(
    props: &TypeProps,
    op_spec: BinOpSpec,
    lhs_spec: OperandTypeSpec,
    rhs_spec: OperandTypeSpec,
) -> quote::Tokens {
    assert!(lhs_spec != OperandTypeSpec::Inner || rhs_spec != OperandTypeSpec::Inner);
    gen_impl_bin_op_sized(
        props,
        op_spec,
        lhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
        rhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
    )
}


pub fn gen_impl_bin_op_sized_ref(
    props: &TypeProps,
    op_spec: BinOpSpec,
    lhs_spec: OperandTypeSpec,
    rhs_spec: OperandTypeSpec,
) -> quote::Tokens {
    assert!(lhs_spec != OperandTypeSpec::Inner || rhs_spec != OperandTypeSpec::Inner);
    let raw_ref = gen_impl_bin_op_sized(
        props,
        op_spec,
        lhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
        rhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
    );
    let ref_raw = gen_impl_bin_op_sized(
        props,
        op_spec,
        lhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
        rhs_spec.with_wrapper(OperandTypeWrapperSpec::Raw),
    );
    let ref_ref = gen_impl_bin_op_sized(
        props,
        op_spec,
        lhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
        rhs_spec.with_wrapper(OperandTypeWrapperSpec::Ref),
    );
    quote! {
        #raw_ref
        #ref_raw
        #ref_ref
    }
}


pub fn gen_impl_bin_op_sized(
    props: &TypeProps,
    op_spec: BinOpSpec,
    lhs_spec: OperandSpec,
    rhs_spec: OperandSpec,
) -> quote::Tokens {
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
            let pred = syn::parse_str::<syn::WherePredicate>(&format!(
                "{}: {}<{}, Output={}>",
                ty_lhs_inner,
                target_trait,
                ty_rhs_inner,
                ty_inner.into_tokens()
            )).expect("Failed to generate `WherePredicate`");
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

    let body = {
        let self_inner = lhs_spec.tokens_inner(props, quote!(self));
        let other_inner = rhs_spec.tokens_inner(props, &other);
        let inner_result = quote! {
            #target_trait::#method_name(
                #self_inner,
                #other_inner
            )
        };
        op_spec.tokens_body(&ty_outer_generic, props.helper_trait(), inner_result)
    };

    quote! {
        impl #impl_generics #target_trait<#ty_rhs_impl> for #ty_lhs_impl
        #where_clause
        {
            #associated

            fn #method_name(#lhs_self_arg, #other: #ty_rhs_arg) -> #ty_ret {
                #body
            }
        }
    }
}
