//! Data about traits to be derived.

use syn::MetaItem;

use names;


/// Auto-derive target trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, AsRefStr)]
pub enum Derive {
    /// `std::ascii::AsciiExt for Outer`.
    AsciiExt,
    /// `AsMut<DerefTarget> for Outer`.
    AsMutDeref,
    /// `AsMut<Inner> for Outer`.
    AsMutInner,
    /// `AsMut<Outer> for Outer`.
    AsMutSelf,
    /// `AsRef<DerefTarget> for Outer`.
    AsRefDeref,
    /// `AsRef<Inner> for Outer`.
    AsRefInner,
    /// `AsRef<Self> for Outer`.
    AsRefSelf,
    /// `std::fmt::Binary for Outer`.
    Binary,
    /// `Default for &Outer`.
    DefaultRef,
    /// `std::ops::Deref for Outer`.
    Deref,
    /// `std::ops::DerefMut for Outer`.
    DerefMut,
    /// `std::fmt::Display for Outer`.
    Display,
    /// `From<Inner> for Outer`.
    FromInner,
    /// `From<Outer> for Arc<Outer>`.
    IntoArc,
    /// `From<Outer> for Box<Outer>`.
    IntoBox,
    /// `From<Outer> for Inner`.
    IntoInner,
    /// `From<Outer> for Rc<Outer>`.
    IntoRc,
    /// `std::fmt::LowerExp for Outer`.
    LowerExp,
    /// `std::fmt::LowerHex for Outer`.
    LowerHex,
    /// `std::fmt::Octal for Outer`.
    Octal,
    /// `PartialEq<Inner> for Outer` and similar ones.
    PartialEqInner,
    /// `PartialEq<Cow<Inner>> for Outer` and similar ones.
    PartialEqInnerCow,
    /// `PartialEq<Cow<Outer>> for Outer` and similar ones.
    PartialEqSelfCow,
    /// `PartialEq<Cow<Outer>> for Inner` and similar ones.
    PartialEqSelfCowAndInner,
    /// `PartialOrd<Inner> for Outer` and similar ones.
    PartialOrdInner,
    /// `PartialOrd<Cow<Inner>> for Outer` and similar ones.
    PartialOrdInnerCow,
    /// `PartialOrd<Cow<Outer>> for Outer` and similar ones.
    PartialOrdSelfCow,
    /// `PartialOrd<Cow<Outer>> for Inner` and similar ones.
    PartialOrdSelfCowAndInner,
    /// `std::fmt::Pointer for Outer`.
    Pointer,
    /// `std::fmt::UpperExp for Outer`.
    UpperExp,
    /// `std::fmt::UpperHex for Outer`.
    UpperHex,
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
                        "Binary" => &[Derive::Binary],
                        "DefaultRef" => &[Derive::DefaultRef],
                        "Deref" => &[Derive::Deref],
                        "DerefMut" => &[Derive::DerefMut],
                        "Display" => &[Derive::Display],
                        "FromInner" => &[Derive::FromInner],
                        "IntoBox" => &[Derive::IntoBox],
                        "IntoArc" => &[Derive::IntoArc],
                        "IntoInner" => &[Derive::IntoInner],
                        "IntoRc" => &[Derive::IntoRc],
                        "LowerExp" => &[Derive::LowerExp],
                        "LowerHex" => &[Derive::LowerHex],
                        "Octal" => &[Derive::Octal],
                        "PartialEqInner" => &[Derive::PartialEqInner],
                        "PartialEqInnerCow" => &[Derive::PartialEqInnerCow],
                        "PartialEqSelfCow" => &[Derive::PartialEqSelfCow],
                        "PartialEqSelfCowAndInner" => &[Derive::PartialEqSelfCowAndInner],
                        "PartialOrdInner" => &[Derive::PartialOrdInner],
                        "PartialOrdInnerCow" => &[Derive::PartialOrdInnerCow],
                        "PartialOrdSelfCow" => &[Derive::PartialOrdSelfCow],
                        "PartialOrdSelfCowAndInner" => &[Derive::PartialOrdSelfCowAndInner],
                        "Pointer" => &[Derive::Pointer],
                        "UpperExp" => &[Derive::UpperExp],
                        "UpperHex" => &[Derive::UpperHex],
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
                    let subtargets = subitems.iter().filter_map(|nested| {
                        if let NestedMetaItem::MetaItem(MetaItem::Word(ref ident)) = *nested {
                            Some(ident.as_ref())
                        } else {
                            None
                        }
                    });
                    let major_target = ident.as_ref();
                    match major_target {
                        "PartialEq" => {
                            derives.extend(subtargets.map(|target| match target {
                                "Inner" => Derive::PartialEqInner,
                                "InnerCow" => Derive::PartialEqInnerCow,
                                "SelfCow" => Derive::PartialEqSelfCow,
                                "SelfCowAndInner" => Derive::PartialEqSelfCowAndInner,
                                _ => panic!(
                                    "Unsupported subtarget: #[opaque_typedef({}({}({})))]",
                                    names::DERIVE,
                                    major_target,
                                    target
                                ),
                            }));
                        },
                        "PartialOrd" => {
                            derives.extend(subtargets.map(|target| match target {
                                "Inner" => Derive::PartialOrdInner,
                                "InnerCow" => Derive::PartialOrdInnerCow,
                                "SelfCow" => Derive::PartialOrdSelfCow,
                                "SelfCowAndInner" => Derive::PartialOrdSelfCowAndInner,
                                _ => panic!(
                                    "Unsupported subtarget: #[opaque_typedef({}({}({})))]",
                                    names::DERIVE,
                                    major_target,
                                    target
                                ),
                            }));
                        },
                        _ => panic!(
                            "`#[opaque_typedef({}({}))]` is specified but the target `{}` is unknown",
                            names::DERIVE,
                            major_target,
                            major_target
                        ),
                    }
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
