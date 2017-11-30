//! Data about traits to be derived.

use syn::MetaItem;

use names;


/// Auto-derive target trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, AsRefStr)]
pub enum Derive {
    AsciiExt,
    AsMutDeref,
    AsMutInner,
    AsRefDeref,
    AsRefInner,
    DefaultRef,
    Deref,
    DerefMut,
    Display,
    FromInner,
    IntoInner,
}

impl Derive {
    /// Get derive targets from the given attributes.
    pub fn from_metaitems(metaitems: &[&MetaItem]) -> Vec<Self> {
        use syn::NestedMetaItem;

        let mut derives = Vec::new();
        let derive_items = metaitems
            .iter()
            .filter_map(|&meta| {
                if let MetaItem::List(ref ident, ref nested) = *meta {
                    if ident == names::DERIVE {
                        return Some(nested);
                    }
                }
                None
            })
            .flat_map(|nested| nested);
        for nested in derive_items {
            match *nested {
                // `derive(ident)` style.
                NestedMetaItem::MetaItem(MetaItem::Word(ref ident)) => {
                    let derive: &[_] = match ident.as_ref() {
                        "AsciiExt" => &[Derive::AsciiExt],
                        "AsMutDeref" => &[Derive::AsMutDeref],
                        "AsMutInner" => &[Derive::AsMutInner],
                        "AsRefDeref" => &[Derive::AsRefDeref],
                        "AsRefInner" => &[Derive::AsRefInner],
                        "DefaultRef" => &[Derive::DefaultRef],
                        "Deref" => &[Derive::Deref],
                        "DerefMut" => &[Derive::DerefMut],
                        "Display" => &[Derive::Display],
                        "FromInner" => &[Derive::FromInner],
                        "IntoInner" => &[Derive::IntoInner],
                        target => panic!(
                            "`#[opaque_typedef({}({}))] is specified but the target `{}` is unknown",
                            names::DERIVE,
                            target,
                            target
                        ),
                    };
                    derives.extend(derive);
                },
                // `derive(ident(subitem1, subitem2, ...))` style.
                NestedMetaItem::MetaItem(MetaItem::List(ref ident, ref subitems)) => {
                    let _ = ident;
                    let _ = subitems;
                    unimplemented!()
                },
                _ => continue,
            }
        }
        derives
    }
}
