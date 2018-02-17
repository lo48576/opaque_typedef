//! Utilities to build `type_props::TypeProps`.

use syn;
use syn::DeriveInput;

use type_props::{Field, Sizedness, TypeProps};


/// A builder of `TypeProps`.
#[derive(Default, Clone)]
pub struct TypePropsBuilder<'a> {
    /// Outer type.
    ty_outer: Option<&'a syn::Ident>,
    /// Inner field.
    field_inner: Option<Field<'a>>,
    /// Sizedness of the inner type.
    inner_sizedness: Option<Sizedness>,
}

impl<'a> TypePropsBuilder<'a> {
    /// Creates a new `TypePropsBuilder`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Loads properties from the given input and sizedness.
    pub fn load(&mut self, input: &'a DeriveInput, sizedness: Sizedness) {
        let _ = input;
        let _ = sizedness;
        unimplemented!();
    }

    /// Builds a `TypeProps`.
    pub fn build(self) -> TypeProps<'a> {
        const MSG_SHOULD_LOAD: &str =
            "Should never happen: `TypePropsBuilder::load()` should be called at least once";
        let ty_outer = self.ty_outer.expect(MSG_SHOULD_LOAD);
        let field_inner = self.field_inner.expect(MSG_SHOULD_LOAD);
        let inner_sizedness = self.inner_sizedness.expect(MSG_SHOULD_LOAD);

        TypeProps {
            ty_outer,
            field_inner,
            inner_sizedness,
        }
    }
}
