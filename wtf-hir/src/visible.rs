use crate::LocalId;

pub struct Visible {
    bindings: Vec<Binding>,
}
// TODO: add mutability
// TODO: make this an enum to register enums and variants here as well here
struct Binding {
    mutable: bool,
    name: String,
    id: LocalId,
}

struct Scope {
    num_bindings: usize,
}

impl Visible {
    pub fn new() -> Self {
        Self { bindings: vec![] }
    }
    pub fn bind(&mut self, name: String, id: LocalId, mutable: bool) {
        self.bindings.push(Binding { mutable, name, id });
    }
    pub fn lookup(&self, name: &str) -> Option<LocalId> {
        self.bindings
            .iter()
            .rev()
            .find(|binding| binding.name == name)
            .map(|binding| binding.id)
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
