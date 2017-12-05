//! Helpers for getting attributes.

use syn::{Attribute, MetaItem};


/// Returns items in `#[attr_name(item1, item2, ...)]`
pub fn get_metaitems<'a>(attrs: &'a [Attribute], attr_name: &str) -> Vec<&'a MetaItem> {
    use syn::NestedMetaItem;

    attrs
        .iter()
        .filter_map(|attr| {
            if let MetaItem::List(ref ident, ref nested) = attr.value {
                if ident == attr_name {
                    return Some(nested);
                }
            }
            None
        })
        .flat_map(|nested| nested)
        .filter_map(|nested| {
            if let NestedMetaItem::MetaItem(ref metaitem) = *nested {
                return Some(metaitem);
            }
            None
        })
        .collect()
}


/// Returns words in `prop_name(word1, word2, ...)`.
pub fn has_word_prop(metaitems: &[&MetaItem], prop_name: &str) -> bool {
    metaitems.iter().any(|&meta| {
        if let MetaItem::Word(ref ident) = *meta {
            ident == prop_name
        } else {
            false
        }
    })
}
