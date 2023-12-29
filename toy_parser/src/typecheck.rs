use crate::ast::Expr;
use rustc_hash::FxHashMap as HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub struct TypeId(usize);

impl TypeId {
    pub fn next(self) -> Self {
        TypeId(self.0 + 1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I32,

    // TODO: no parser for this
    Fn {
        params: Vec<Type>,
        return_ty: Box<Type>,
    },
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::I32 => "i32",
            _ => unimplemented!(),
        }
    }
}

#[derive(Default)]
pub struct Types {
    pub types: HashMap<TypeId, Type>,
    next_id: TypeId,
}

impl Types {
    pub fn get_by_name(&self, name: &str) -> Result<TypeId, Error> {
        self.types
            .iter()
            .find(|(_, ty)| ty.name() == name)
            .map(|(&id, _)| id)
            .ok_or_else(|| Error {
                message: format!("Type not found: {}", name),
            })
    }

    pub fn get_or_insert(&mut self, ty: Type) -> TypeId {
        self.types
            .iter()
            .find(|(_, &t)| t == ty)
            .map(|(&id, _)| id)
            .unwrap_or_else(|| self.insert(ty))
    }

    fn insert(&mut self, ty: Type) -> TypeId {
        let id = self.next_id;
        self.next_id = self.next_id.next();
        self.types.insert(id, ty);
        id
    }
}

pub struct Error {
    pub message: String,
}

pub fn typecheck_expr(expr: &Expr, types: &mut Types) -> Result<TypeId, Error> {}
