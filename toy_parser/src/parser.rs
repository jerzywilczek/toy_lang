use std::sync::{Arc, Mutex};

use crate::{
    ast::*,
    typecheck::{self, TypeId, Types},
};

use chumsky::prelude::*;

pub fn parse(source: &str) -> Result<Module, Vec<Simple<char>>> {
    let types = Arc::new(Mutex::new(typecheck::Types::default()));

    let module = module_parser(types).parse(source)?;

    Ok(module)
}

fn module_parser(types: Arc<Mutex<Types>>) -> impl Parser<char, Module, Error = Simple<char>> {
    let module_header = text::keyword("module")
        .padded()
        .ignore_then(text::ident())
        .padded()
        .then_ignore(just(";"));

    let fn_def = fn_def_parser(types).padded().repeated();

    module_header
        .then(fn_def)
        .then_ignore(end())
        .map(|(name, functions)| Module { name, functions })
}

fn fn_def_parser(types: Arc<Mutex<Types>>) -> impl Parser<char, FnDef, Error = Simple<char>> {
    text::keyword("fn")
        .padded()
        .ignore_then(text::ident())
        .padded()
        .then_ignore(just("("))
        .then(
            fn_param_parser(types.clone())
                .padded()
                .separated_by(just(","))
                .allow_trailing(),
        )
        .then_ignore(just(")"))
        .padded()
        .then_ignore(just(":"))
        .padded()
        .then(typename_parser(types))
        .padded()
        .then_ignore(just("{"))
        .padded()
        .then(statement_parser().padded().repeated())
        .then_ignore(just("}"))
        .map(|(((name, params), return_ty), body)| FnDef {
            name,
            params,
            return_ty,
            body,
        })
}

fn fn_param_parser(types: Arc<Mutex<Types>>) -> impl Parser<char, FnParam, Error = Simple<char>> {
    text::ident()
        .padded()
        .then_ignore(just(":"))
        .padded()
        .then(typename_parser(types))
        .map(|(name, ty)| FnParam { name, ty })
}

fn typename_parser(types: Arc<Mutex<Types>>) -> impl Parser<char, TypeId, Error = Simple<char>> {
    text::keyword("i32").map_with_span(|_, s| s).try_map(|s| {
        types
            .lock()
            .unwrap()
            .get_by_name(s)
            .map_err(|_| Simple::custom(s, "Type not found"))
    })
}

fn statement_parser() -> impl Parser<char, Statement, Error = Simple<char>> {
    let return_parser = text::keyword("return")
        .padded()
        .ignore_then(expr_parser())
        .then_ignore(just(";"))
        .map(Statement::Return);

    return_parser
}

fn expr_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let imm = text::int(10)
            .map(|s: String| Expr::I32(s.parse().unwrap()))
            .padded();

        let ident = text::ident().map(Expr::Identifier).padded();

        let atom = imm
            .or(ident)
            .or(expr.delimited_by(just("("), just(")")))
            .padded();

        let op = |c| just(c).padded();

        let unary = op("-")
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::UnExpr(UnExpr::Neg(Box::new(rhs))));

        let mul = unary
            .clone()
            .then(
                op("*")
                    .to(
                        (|lhs, rhs| Expr::BinExpr(BinExpr::Mul(Box::new(lhs), Box::new(rhs))))
                            as fn(_, _) -> _,
                    )
                    .or(op("/").to((|lhs, rhs| {
                        Expr::BinExpr(BinExpr::Div(Box::new(lhs), Box::new(rhs)))
                    }) as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(lhs, rhs));

        mul.clone()
            .then(
                op("+")
                    .to(
                        (|lhs, rhs| Expr::BinExpr(BinExpr::Add(Box::new(lhs), Box::new(rhs))))
                            as fn(_, _) -> _,
                    )
                    .or(op("-").to((|lhs, rhs| {
                        Expr::BinExpr(BinExpr::Sub(Box::new(lhs), Box::new(rhs)))
                    }) as fn(_, _) -> _))
                    .then(mul)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(lhs, rhs))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_program() {
        let src = r#"
            module simple;

            fn test(i: i32): i32 {
                return (-i + 1) * 2 + 3;
            }
        "#;

        let ast = Module {
            name: "simple".to_string(),
            functions: vec![FnDef {
                name: "test".to_string(),
                params: vec![FnParam {
                    name: "i".to_string(),
                    ty: Type::I32,
                }],
                return_ty: Type::I32,
                body: vec![Statement::Return(Expr::BinExpr(BinExpr::Add(
                    Box::new(Expr::BinExpr(BinExpr::Mul(
                        Box::new(Expr::BinExpr(BinExpr::Add(
                            Box::new(Expr::UnExpr(UnExpr::Neg(Box::new(Expr::Identifier(
                                "i".to_string(),
                            ))))),
                            Box::new(Expr::I32(1)),
                        ))),
                        Box::new(Expr::I32(2)),
                    ))),
                    Box::new(Expr::I32(3)),
                )))],
            }],
        };

        assert_eq!(parse(src).unwrap(), ast);
    }

    #[test]
    fn parse_ident_return() {
        let src = r#"
            module simple;

            fn test(i: i32): i32 {
                return i;
            }
        "#;

        let ast = Module {
            name: "simple".to_string(),
            functions: vec![FnDef {
                name: "test".to_string(),
                params: vec![FnParam {
                    name: "i".to_string(),
                    ty: Type::I32,
                }],
                return_ty: Type::I32,
                body: vec![Statement::Return(Expr::Identifier("i".to_string()))],
            }],
        };

        assert_eq!(parse(src).unwrap(), ast);
    }
}
