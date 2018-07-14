//! Utilities to build `type_props::TypeProps`.

use quote::ToTokens;
use syn;
use syn::DeriveInput;

use attrs::{get_meta_content_by_path, has_word_meta, is_attr_with_path};
use derives::Derive;
use type_props::{CmpSpec, DerefSpec, Field, Sizedness, TypeProps, ValidationSpec};
use utils::expect_singleton_iter;

/// Returns `#[repr(..)]` metadata.
fn get_repr_meta(attrs: &[syn::Attribute]) -> Option<syn::Meta> {
    let iter = attrs
        .into_iter()
        .filter(|attr| is_attr_with_path(attr, &["repr"]))
        .filter_map(|attr| attr.interpret_meta());
    expect_singleton_iter(iter)
        .at_most_one()
        .expect("Multiple `#[repr(..)]` are not supported")
}

/// Returns a field marked (explicitly or implicitly) as "inner".
fn get_inner_field(data: &syn::Data) -> Field {
    // Currently, only struct is supported.
    let fields = match *data {
        syn::Data::Struct(ref data) => &data.fields,
        syn::Data::Enum(..) => panic!("Enum types are not supported"),
        syn::Data::Union(..) => panic!("Currently, union types are not supported"),
    };
    // Get fields with `syn::Fields` type.
    let fields = match *fields {
        syn::Fields::Named(ref fields) => &fields.named,
        syn::Fields::Unnamed(ref fields) => &fields.unnamed,
        syn::Fields::Unit => panic!("Types with no fields are not supported"),
    };
    // If there is only one field, it is the inner field.
    if fields.len() == 1 {
        let field = &fields[0];
        if field.ident.is_some() {
            return Field::Named(field);
        } else {
            return Field::Unnamed(field, 0);
        }
    }
    if fields.len() > 1 {
        panic!("Currently, outer type with multiple fields is not supported");
    } else if fields.is_empty() {
        panic!("Types with no fields are not supported");
    }
    unreachable!("Currently, outer types with multiple fields are not supported");
}

fn check_repr_outer(
    ty_outer: &syn::Ident,
    sizedness: Sizedness,
    repr_meta_outer: Option<&syn::Meta>,
) {
    if sizedness != Sizedness::Unsized {
        // The restriction is necessary only for unsized types.
        return;
    }
    if let Some(repr_meta) = repr_meta_outer {
        let has_repr_c = has_word_meta(repr_meta, &["repr", "C"]);
        let has_repr_transparent = has_word_meta(repr_meta, &["repr", "transparent"]);
        if has_repr_c || has_repr_transparent {
            return;
        }
    }
    // Neither `repr(C)` nor `repr(transparent)` was specified for an unsized type.
    panic!(
        "To avoid undefined behavior, outer type `{}` should be marked \
         as `#[repr(C)]` or `#[repr(transparent)]`.\n\
         For detail, see <https://github.com/lo48576/opaque_typedef/issues/1>.\n\
         About `#[repr(transparent)]`, see RFC 1758 \
         <https://github.com/rust-lang/rfcs/blob/master/text/1758-repr-transparent.md>.\
         ",
        ty_outer
    );
}

fn get_deref_spec(attrs: &[syn::Attribute]) -> DerefSpec {
    let namevalues = attrs
        .into_iter()
        .filter(|attr| is_attr_with_path(attr, &["opaque_typedef"]))
        .filter_map(|attr| attr.interpret_meta())
        .flat_map(|meta| get_meta_content_by_path(meta, &["opaque_typedef", "deref"]))
        .filter_map(|meta| match meta {
            syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => Some(nv),
            _ => None,
        })
        .collect::<Vec<_>>();

    fn get_attr_by_name<'a>(
        namevalues: &'a [syn::MetaNameValue],
        name: &str,
    ) -> Option<&'a syn::LitStr> {
        let iter = namevalues
            .into_iter()
            .filter(|nv| nv.ident == name)
            .map(|nv| &nv.lit);
        let lit = expect_singleton_iter(iter)
            .at_most_one()
            .unwrap_or_else(|| {
                panic!(
                    "`#[opaque_typedef(deref({} = ..))]` can be specified at \
                     most once for each type",
                    name
                )
            })?;
        match *lit {
            syn::Lit::Str(ref s) => Some(s),
            ref lit => panic!(
                "String value is expected for `#[opaque_typedef(deref({} = ..))]`, \
                 but got `{}` (invalid type)",
                name,
                lit.into_token_stream()
            ),
        }
    }

    let target = get_attr_by_name(&namevalues, "target");
    let deref = get_attr_by_name(&namevalues, "deref");
    let deref_mut = get_attr_by_name(&namevalues, "deref_mut");
    let ty_deref_target = target.map(|target| {
        target.parse::<syn::Type>().unwrap_or_else(|e| {
            panic!(
                "`#[opaque_typedef(deref(target = ..))]` is specified \
                 but failed to parse `{}` as type: {}",
                target.value(),
                e
            )
        })
    });
    let fn_name_deref = deref.map(|deref| {
        deref.parse::<syn::Expr>().unwrap_or_else(|e| {
            panic!(
                "`#[opaque_typedef(deref(deref = ..))]` is specified \
                 but failed to parse `{}` as expression: {}",
                deref.value(),
                e
            )
        })
    });
    let fn_name_deref_mut = deref_mut.map(|deref_mut| {
        deref_mut.parse::<syn::Expr>().unwrap_or_else(|e| {
            panic!(
                "`#[opaque_typedef(deref(deref_mut = ..))]` is \
                 specified but failed to parse `{}` as expression: {}",
                deref_mut.value(),
                e
            )
        })
    });
    DerefSpec {
        ty_deref_target,
        fn_name_deref,
        fn_name_deref_mut,
    }
}

fn get_mut_ref_allowed(attrs: &[syn::Attribute]) -> bool {
    attrs
        .into_iter()
        .filter(|attr| is_attr_with_path(attr, &["opaque_typedef"]))
        .filter_map(|attr| attr.interpret_meta())
        .any(|meta| has_word_meta(&meta, &["opaque_typedef", "allow_mut_ref"]))
}

fn get_validation_spec(attrs: &[syn::Attribute]) -> ValidationSpec {
    let namevalues = attrs
        .into_iter()
        .filter(|attr| is_attr_with_path(attr, &["opaque_typedef"]))
        .filter_map(|attr| attr.interpret_meta())
        .flat_map(|meta| get_meta_content_by_path(meta, &["opaque_typedef", "validation"]))
        .filter_map(|meta| match meta {
            syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => Some(nv),
            _ => None,
        })
        .collect::<Vec<_>>();

    fn get_attr_by_name<'a>(
        namevalues: &'a [syn::MetaNameValue],
        name: &str,
    ) -> Option<&'a syn::LitStr> {
        let iter = namevalues
            .into_iter()
            .filter(|nv| nv.ident == name)
            .map(|nv| &nv.lit);
        let lit = expect_singleton_iter(iter)
            .at_most_one()
            .unwrap_or_else(|| {
                panic!(
                    "`#[opaque_typedef(validation({} = ..))]` can be specified \
                     at most once for each type",
                    name
                )
            })?;
        match *lit {
            syn::Lit::Str(ref s) => Some(s),
            ref lit => panic!(
                "String value is expected for `#[opaque_typedef(validation({} = ..))]`, \
                 but got `{}` (invalid type)",
                name,
                lit.into_token_stream()
            ),
        }
    }

    let fn_validator = get_attr_by_name(&namevalues, "validator").map(|litstr| {
        litstr.parse::<syn::Expr>().unwrap_or_else(|e| {
            panic!(
                "`#[opaque_typedef(validation(validator = ..))]` is specified \
                 but failed to parse `{}` as expression: {}",
                litstr.value(),
                e
            )
        })
    });
    let ty_error = get_attr_by_name(&namevalues, "error_type").map(|litstr| {
        litstr.parse::<syn::Type>().unwrap_or_else(|e| {
            panic!(
                "`#[opaque_typedef(validation(error_type = ..))]` is specified \
                 but failed to parse `{}` as expression: {}",
                litstr.value(),
                e
            )
        })
    });
    let error_msg = get_attr_by_name(&namevalues, "error_msg").map(|litstr| litstr.value());

    match (fn_validator.is_some(), ty_error.is_some()) {
        (true, false) => panic!(
            "`#[opaque_typedef(validation(validator = ..))]` is specified but \
             `#[opaque_typedef(validation(error_type = ..))]` is not found"
        ),
        (false, true) => panic!(
            "`#[opaque_typedef(validation(error_type = ..))]` is specified but \
             `#[opaque_typedef(validation(validator = ..))]` is not found"
        ),
        _ => {},
    }

    ValidationSpec {
        fn_validator,
        ty_error,
        error_msg,
    }
}

fn get_cmp_spec(attrs: &[syn::Attribute]) -> CmpSpec {
    let namevalues = attrs
        .into_iter()
        .filter(|attr| is_attr_with_path(attr, &["opaque_typedef"]))
        .filter_map(|attr| attr.interpret_meta())
        .flat_map(|meta| get_meta_content_by_path(meta, &["opaque_typedef", "cmp"]))
        .filter_map(|meta| match meta {
            syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => Some(nv),
            _ => None,
        })
        .collect::<Vec<_>>();

    fn get_attr_by_name<'a>(
        namevalues: &'a [syn::MetaNameValue],
        name: &str,
    ) -> Option<&'a syn::LitStr> {
        let iter = namevalues
            .into_iter()
            .filter(|nv| nv.ident == name)
            .map(|nv| &nv.lit);
        let lit = expect_singleton_iter(iter)
            .at_most_one()
            .unwrap_or_else(|| {
                panic!(
                    "`#[opaque_typedef(cmp({} = ..))]` can be specified \
                     at most once for each type",
                    name
                )
            })?;
        match *lit {
            syn::Lit::Str(ref s) => Some(s),
            ref lit => panic!(
                "String value is expected for `#[opaque_typedef(cmp({} = ..))]`, \
                 but got `{}` (invalid type)",
                name,
                lit.into_token_stream()
            ),
        }
    }

    let partial_eq = get_attr_by_name(&namevalues, "partial_eq").map(|litstr| {
        litstr.parse::<syn::Expr>().unwrap_or_else(|e| {
            panic!(
                "`#[opaque_typedef(cmp(partial_eq = ..))]` is specified \
                 but failed to parse `{}` as expression: {}",
                litstr.value(),
                e
            )
        })
    });

    let partial_ord = get_attr_by_name(&namevalues, "partial_ord").map(|litstr| {
        litstr.parse::<syn::Expr>().unwrap_or_else(|e| {
            panic!(
                "`#[opaque_typedef(cmp(partial_ord = ..))]` is specified \
                 but failed to parse `{}` as expression: {}",
                litstr.value(),
                e
            )
        })
    });

    let ord = get_attr_by_name(&namevalues, "ord").map(|litstr| {
        litstr.parse::<syn::Expr>().unwrap_or_else(|e| {
            panic!(
                "`#[opaque_typedef(cmp(ord = ..))]` is specified \
                 but failed to parse `{}` as expression: {}",
                litstr.value(),
                e
            )
        })
    });

    CmpSpec {
        partial_eq,
        partial_ord,
        ord,
    }
}

/// A builder of `TypeProps`.
#[derive(Default, Clone)]
pub struct TypePropsBuilder<'a> {
    /// Outer type.
    ty_outer: Option<&'a syn::Ident>,
    /// `#[repr(..)]` spec of the outer type.
    repr_attr_outer: Option<syn::Meta>,
    /// Inner field.
    field_inner: Option<Field<'a>>,
    /// Generics.
    generics: Option<&'a syn::Generics>,
    /// Sizedness of the inner type.
    inner_sizedness: Option<Sizedness>,
    /// Derive target traits.
    derives: Option<Vec<Derive>>,
    /// Deref spec.
    deref_spec: Option<DerefSpec>,
    /// Whether the mutable reference to the inner field is allowed.
    is_mut_ref_allowed: Option<bool>,
    /// Validation spec.
    validation_spec: Option<ValidationSpec>,
    /// Cmp spec.
    cmp_spec: Option<CmpSpec>,
}

impl<'a> TypePropsBuilder<'a> {
    /// Creates a new `TypePropsBuilder`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Loads properties from the given input and sizedness.
    pub fn load(&mut self, input: &'a DeriveInput, sizedness: Sizedness) {
        self.ty_outer = Some(&input.ident);
        self.repr_attr_outer = get_repr_meta(&input.attrs);
        self.field_inner = Some(get_inner_field(&input.data));
        self.generics = Some(&input.generics);
        self.inner_sizedness = Some(sizedness);
        self.derives = Some(Derive::from_attrs(&input.attrs));
        self.deref_spec = Some(get_deref_spec(&input.attrs));
        self.is_mut_ref_allowed = Some(get_mut_ref_allowed(&input.attrs));
        self.validation_spec = Some(get_validation_spec(&input.attrs));
        self.cmp_spec = Some(get_cmp_spec(&input.attrs));
    }

    /// Builds a `TypeProps`.
    pub fn build(self) -> TypeProps<'a> {
        const MSG_SHOULD_LOAD: &str =
            "Should never happen: `TypePropsBuilder::load()` should be called at least once";
        let ty_outer = self.ty_outer.expect(MSG_SHOULD_LOAD);
        let field_inner = self.field_inner.expect(MSG_SHOULD_LOAD);
        let generics = self.generics.expect(MSG_SHOULD_LOAD);
        let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
        let inner_sizedness = self.inner_sizedness.expect(MSG_SHOULD_LOAD);
        check_repr_outer(ty_outer, inner_sizedness, self.repr_attr_outer.as_ref());
        let derives = self.derives.expect(MSG_SHOULD_LOAD);
        let deref_spec = self.deref_spec.expect(MSG_SHOULD_LOAD);
        let is_mut_ref_allowed = self.is_mut_ref_allowed.expect(MSG_SHOULD_LOAD);
        let validation_spec = self.validation_spec.expect(MSG_SHOULD_LOAD);
        let cmp_spec = self.cmp_spec.expect(MSG_SHOULD_LOAD);

        TypeProps {
            ty_outer,
            field_inner,
            generics,
            impl_generics,
            type_generics,
            where_clause,
            inner_sizedness,
            derives,
            deref_spec,
            is_mut_ref_allowed,
            validation_spec,
            cmp_spec,
        }
    }
}
