//! Functions to get field data.

use syn;

use attrs;
use names;

/// Returns the inner field and its order.
///
/// Panics if the inner field was not found or multiple fields are marked as inner.
pub fn get_inner_field<'a>(ast: &'a syn::DeriveInput) -> (&syn::Field, usize) {
    let variant_data = match ast.body {
        syn::Body::Enum(..) => {
            panic!("`#[derive(OpaqueTypedef)]` is currently not supported for enums")
        },
        syn::Body::Struct(ref variant_data) => variant_data,
    };
    let fields = match *variant_data {
        syn::VariantData::Struct(ref fields) | syn::VariantData::Tuple(ref fields) => fields,
        syn::VariantData::Unit => panic!("#[derive(CustomSlice)] is not supported for unit struct"),
    };
    assert!(fields.len() >= 1, "The type should have one or more fields");

    if fields.len() == 1 {
        // There is only one field. Gotcha!
        return (&fields[0], 0);
    }

    let mut fields_iter = fields.into_iter();
    // Find the first field marked as inner.
    let first_inner = fields_iter
        .by_ref()
        .enumerate()
        .find(|&(_order, ref field)| {
            let attrs = attrs::get_metaitems(&field.attrs, names::ATTR_NAME);
            attrs::has_word_prop(&attrs, names::INNER_FIELD)
        });
    if let Some((order, ref inner)) = first_inner {
        // Ensure the other fields are not marked as inner.
        let no_other_inners = fields_iter.all(|field| {
            let attrs = attrs::get_metaitems(&field.attrs, names::ATTR_NAME);
            !attrs::has_word_prop(&attrs, names::INNER_FIELD)
        });
        assert!(
            no_other_inners,
            "`#[{}({})]` can be set to at most one field",
            names::ATTR_NAME,
            names::INNER_FIELD
        );
        (inner, order)
    } else {
        // There are multiple fields but none of them are marked as inner.
        panic!(
            "`#[{}({})]` should be set to one of the fields if the type has multiple fields",
            names::ATTR_NAME,
            names::INNER_FIELD
        );
    }
}


/// Finds inner field and returns name and type of it.
pub fn get_inner_name_and_ty(ast: &syn::DeriveInput) -> (syn::Ident, &syn::Ty) {
    let (inner_field, inner_order) = get_inner_field(ast);
    let name = inner_field.ident.clone().unwrap_or(inner_order.into());
    (name, &inner_field.ty)
}
