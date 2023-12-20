use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, i32, multispace0, multispace1},
    combinator::{map, recognize, value},
    multi::{many0, many0_count, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use crate::ast::*;

type Result<'a, T> = nom::IResult<&'a str, T, nom::error::VerboseError<&'a str>>;

pub fn parse(source: &str) -> anyhow::Result<Module> {
    let (_, module) = parse_module(source).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;

    Ok(module)
}

fn parse_module(input: &str) -> Result<Module> {
    let (input, (_mod, name, _sc, functions)) = tuple((
        delimited(multispace0, tag("module"), multispace1),
        terminated(parse_identifier, multispace0),
        tag(";"),
        separated_list0(multispace0, parse_fn_def),
    ))(input)?;

    Ok((input, Module { name, functions }))
}

fn parse_identifier(input: &str) -> Result<String> {
    let (input, id) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)?;

    println!("parsed identifier {}", id);

    Ok((input, id.to_string()))
}

fn parse_fn_def(input: &str) -> Result<FnDef> {
    println!("parse_fn_def({:#?})", input);
    
    let (input, (_fn, name, params, return_ty, statements)) = tuple((
        delimited(multispace0, tag("fn"), multispace1),
        terminated(parse_identifier, multispace0),
        delimited(
            tag("("),
            separated_list0(tag(","), ws(parse_fn_param)),
            tag(")"),
        ),
        preceded(ws(tag(":")), ws(parse_type)),
        parse_fn_body,
    ))(input)?;

    Ok((
        input,
        FnDef {
            name,
            return_ty,
            params,
            body: statements,
        },
    ))
}

fn parse_fn_body(input: &str) -> Result<Vec<Statement>> {
    println!("parse_fn_body({:?})", input);

    let (input, statements) = delimited(
        tag("{"),
        many0(ws(parse_statement)),
        tag("}"),
    )(input)?;

    Ok((input, statements))
}

fn parse_fn_param(input: &str) -> Result<FnParam> {
    let (input, (name, _c, ty)) = tuple((
        parse_identifier,
        terminated(tag(":"), multispace0),
        parse_type,
    ))(input)?;

    Ok((input, FnParam { name, ty }))
}

fn parse_type(input: &str) -> Result<Type> {
    alt((value(Type::I32, tag("i32")),))(input)
}

fn parse_statement(input: &str) -> Result<Statement> {
    println!("parse_statement({:?})", input);

    alt((
        parse_return,
        map(terminated(parse_expr, tag(";")), Statement::Expr),
    ))(input)
}

fn parse_return(input: &str) -> Result<Statement> {
    let (input, (_r, expr, _sc)) = tuple((
        delimited(multispace0, tag("return"), multispace1),
        ws(parse_expr),
        tag(";"),
    ))(input)?;

    Ok((input, Statement::Return(expr)))
}

fn parse_expr(input: &str) -> Result<Expr> {
    println!("parse_expr({:?})", input);

    alt((parse_identifier_expr, parse_literal_expr, parse_multiply))(input)
}

fn parse_multiply(input: &str) -> Result<Expr> {
    println!("parse_multiply({:?})", input);

    let (input, (lhs, _op, rhs)) = tuple((parse_expr, ws(tag("*")), parse_expr))(input)?;

    Ok((input, Expr::Multiply(Box::new(lhs), Box::new(rhs))))
}

fn parse_identifier_expr(input: &str) -> Result<Expr> {
    let (input, id) = parse_identifier(input)?;

    Ok((input, Expr::Identifier(id)))
}

fn parse_literal_expr(input: &str) -> Result<Expr> {
    let (input, lit) = alt((map(i32, Expr::I32),))(input)?;

    Ok((input, lit))
}

fn ws<'a, F: 'a, O, E: nom::error::ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_program() {
        let source = include_str!("../code_samples/simple_program.tl");

        let ast = Module {
            name: "simple".into(),
            functions: vec![FnDef {
                name: "test".into(),
                return_ty: Type::I32,
                params: vec![FnParam {
                    name: "i".into(),
                    ty: Type::I32,
                }],
                body: vec![Statement::Return(Expr::Multiply(
                    Box::new(Expr::Identifier("i".into())),
                    Box::new(Expr::I32(2)),
                ))],
            }],
        };

        assert_eq!(parse(source).unwrap(), ast);
    }

    #[test]
    fn parse_fn_def_test() {
        let source = "fn test(i: i32) { return i * 2; }";

        parse_fn_def(source).unwrap();
    }
}
