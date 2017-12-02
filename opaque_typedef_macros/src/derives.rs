//! Data about traits to be derived.

use syn::MetaItem;

use names;


/// Auto-derive target trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, AsRefStr)]
pub enum Derive {
    AsciiExt,
    AsMutDeref,
    AsMutInner,
    AsMutSelf,
    AsRefDeref,
    AsRefInner,
    AsRefSelf,
    DefaultRef,
    Deref,
    DerefMut,
    Display,
    FromInner,
    IntoArc,
    IntoBox,
    IntoInner,
    IntoRc,
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
                        "AsMutSelf" => &[Derive::AsMutSelf],
                        "AsRefDeref" => &[Derive::AsRefDeref],
                        "AsRefInner" => &[Derive::AsRefInner],
                        "AsRefSelf" => &[Derive::AsRefSelf],
                        "DefaultRef" => &[Derive::DefaultRef],
                        "Deref" => &[Derive::Deref],
                        "DerefMut" => &[Derive::DerefMut],
                        "Display" => &[Derive::Display],
                        "FromInner" => &[Derive::FromInner],
                        "IntoBox" => &[Derive::IntoBox],
                        "IntoArc" => &[Derive::IntoArc],
                        "IntoInner" => &[Derive::IntoInner],
                        "IntoRc" => &[Derive::IntoRc],
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

    /// Returns whether the target trait requires mutable reference to the inner type.
    pub fn requires_mut_inner(&self) -> bool {
        match *self {
            Derive::AsciiExt | Derive::AsMutDeref | Derive::AsMutInner | Derive::DerefMut => true,
            _ => false,
        }
    }
}
