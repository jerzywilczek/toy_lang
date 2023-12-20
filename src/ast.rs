#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub name: String,
    pub functions: Vec<FnDef>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expr(Expr),
    Return(Expr),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    I32(i32),
    Identifier(String),
    Multiply(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<FnParam>,
    pub return_ty: Type,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I32,
}
