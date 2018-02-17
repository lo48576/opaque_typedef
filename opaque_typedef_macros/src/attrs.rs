//! Utilities for attributes.

use syn;


/// Checks whether the attribute has path with the given toplevel segment.
pub fn is_attr_for(attr: &syn::Attribute, toplevel: &str) -> bool {
    attr.path
        .segments
        .first()
        .map(|seg| seg.value().ident.as_ref()) == Some(toplevel)
}


/// Checks whether the word meta item with the given path is specified.
pub fn has_word_meta(meta: &syn::Meta, ident_path: &[&str]) -> bool {
    match *meta {
        syn::Meta::Word(ref ident) => ident_path.len() == 1 && ident.as_ref() == ident_path[0],
        syn::Meta::List(ref metalist) => {
            if ident_path.len() > 1 && metalist.ident.as_ref() == ident_path[0] {
                metalist
                    .nested
                    .iter()
                    .filter_map(|nested_meta| match *nested_meta {
                        syn::NestedMeta::Meta(ref meta) => Some(meta),
                        syn::NestedMeta::Literal(..) => None,
                    })
                    .any(|meta| has_word_meta(meta, &ident_path[1..]))
            } else {
                false
            }
        },
        syn::Meta::NameValue(..) => false,
    }
}
