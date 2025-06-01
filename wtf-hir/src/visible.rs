use std::collections::HashMap;

use crate::{Type, VarId};

pub struct Visible<'a> {
    bindings: Vec<Binding>,
    types: &'a HashMap<String, Type>,
}
// TODO: make this an enum to register enums and variants here as well here?
#[derive(Clone)]
pub struct Binding {
    pub name: String,
    pub id: VarId,
    pub mutable: bool,
}

pub struct Scope {
    num_bindings: usize,
}

impl<'a> Visible<'a> {
    pub fn new(with: &'a HashMap<String, Type>) -> Self {
        Self {
            bindings: Vec::new(),
            types: with,
        }
    }

    pub fn bind(&mut self, name: String, id: VarId, mutable: bool) {
        self.bindings.push(Binding { name, id, mutable });
    }

    pub fn lookup(&self, name: &str) -> Option<Binding> {
        self.bindings
            .iter()
            .rev()
            .find(|binding| binding.name == name)
            .cloned()
    }

    pub fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    pub fn snapshot(&self) -> Scope {
        Scope {
            num_bindings: self.bindings.len(),
        }
    }

    pub fn restore(&mut self, scope: Scope) {
        self.bindings.truncate(scope.num_bindings);
    }
}
