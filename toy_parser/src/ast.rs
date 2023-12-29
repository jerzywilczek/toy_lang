use crate::typecheck::TypeId;

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
    BinExpr(BinExpr),
    UnExpr(UnExpr),
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinExpr {
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnExpr {
    Neg(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<FnParam>,
    pub return_ty: TypeId,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnParam {
    pub name: String,
    pub ty: TypeId,
}
