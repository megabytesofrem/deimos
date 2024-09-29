#![cfg(test)]
use crate::syntax::ast::{Literal, Member, Stmt};
use crate::syntax::ast_types::{SizedNumber, Ty};
use crate::syntax::{ast::Expr, lexer::Op, parser::Parser};

fn parser(src: &str) -> Parser {
    Parser::new(crate::syntax::lexer::lex_tokens(src))
}

pub(crate) fn parse_expr(src: &str) -> Expr {
    // We don't actually care about the result, so long as it is Ok
    parser(src)
        .parse_expr()
        .expect("Failed to parse expression")
        .target
}

pub(crate) fn parse_stmt(src: &str) -> Stmt {
    // We don't actually care about the result, so long as it is Ok
    parser(src)
        .parse_stmt()
        .expect("Failed to parse statement")
        .target
}

// Expression parsing tests

#[test]
fn comments_should_be_ignored() {
    let mut parser = parser("let x: i32 = 1 + 2 -- this is a comment");
    let parsed = parser.parse_stmt().unwrap().target;

    assert!(matches!(parsed, Stmt::Let { .. }));
}

#[test]
fn simple_expr() {
    let parsed = parse_expr("1 + 2").strip_span();

    let expected = Expr::BinOp(
        Box::new(Expr::Literal(Literal::Int(1)).spanned_default()),
        Op::Add,
        Box::new(Expr::Literal(Literal::Int(2)).spanned_default()),
    );

    assert_eq!(parsed, expected)
}

#[test]
fn expr_precedence_nesting() {
    let parsed = parse_expr("1 + 2 * 3").strip_span();

    let expected = Expr::BinOp(
        Box::new(Expr::Literal(Literal::Int(1)).spanned_default()),
        Op::Add,
        Box::new(
            Expr::BinOp(
                Box::new(Expr::Literal(Literal::Int(2)).spanned_default()),
                Op::Mul,
                Box::new(Expr::Literal(Literal::Int(3)).spanned_default()),
            )
            .spanned_default(),
        ),
    );

    assert_eq!(parsed, expected)
}

#[test]
fn member_access() {
    let parsed = parse_expr("foo.bar").strip_span();

    let expected = Expr::Member(Member {
        target: Box::new(Expr::Ident("foo".to_string()).spanned_default()),
        name: "bar".to_string(),
    });

    assert_eq!(parsed, expected)
}

#[test]
fn member_function_call() {
    let parsed = parse_expr("foo.bar(baz)").strip_span();

    let expected = Expr::Call {
        callee: Box::new(
            Expr::Member(Member {
                target: Box::new(Expr::Ident("foo".to_string()).spanned_default()),
                name: "bar".to_string(),
            })
            .spanned_default(),
        ),
        args: vec![Expr::Ident("baz".to_string()).spanned_default()],
    };

    assert_eq!(parsed, expected)
}

#[test]
fn function_call() {
    let parsed = parse_expr("foo(bar, baz)").strip_span();

    let expected = Expr::Call {
        callee: Box::new(Expr::Ident("foo".to_string()).spanned_default()),
        args: vec![
            Expr::Ident("bar".to_string()).spanned_default(),
            Expr::Ident("baz".to_string()).spanned_default(),
        ],
    };

    assert_eq!(parsed, expected)
}

#[test]
fn array_literal() {
    let parsed = parse_expr("[1, 2, 3]").strip_span();

    let expected = Expr::Array(vec![
        Expr::Literal(Literal::Int(1)).spanned_default(),
        Expr::Literal(Literal::Int(2)).spanned_default(),
        Expr::Literal(Literal::Int(3)).spanned_default(),
    ]);

    assert_eq!(parsed, expected)
}

#[test]
fn cast_expr() {
    let parsed = parse_expr("cast(foo, i32)").strip_span();

    let expected = Expr::Cast(
        Box::new(Expr::Ident("foo".to_string()).spanned_default()),
        Ty::Number(SizedNumber::I32),
    );

    assert_eq!(parsed, expected)
}

#[test]
fn struct_constructor() {
    let parsed = parse_expr("{bar:1, baz:2}").strip_span();

    let expected = Expr::StructCons {
        fields: vec![
            (
                "bar".to_string(),
                Expr::Literal(Literal::Int(1)).spanned_default(),
            ),
            (
                "baz".to_string(),
                Expr::Literal(Literal::Int(2)).spanned_default(),
            ),
        ],
    };

    assert_eq!(parsed, expected)
}

// Statement parsing tests

#[test]
fn let_stmt() {
    let parsed = parse_stmt("let foo: i32 = 1").strip_span();

    let expected = Stmt::Let {
        name: "foo".to_string(),
        ty: Ty::Number(SizedNumber::I32).into(),
        value: Some(Expr::Literal(Literal::Int(1)).spanned_default()),
    };

    assert_eq!(parsed, expected)
}

#[test]
fn assign_stmt() {
    let parsed = parse_stmt("name = 1 + 2").strip_span();

    let expected = Stmt::Assign {
        name: Expr::Ident("name".to_string()).spanned_default(),
        value: Expr::BinOp(
            Box::new(Expr::Literal(Literal::Int(1)).spanned_default()),
            Op::Add,
            Box::new(Expr::Literal(Literal::Int(2)).spanned_default()),
        )
        .spanned_default(),
    };

    assert_eq!(parsed, expected)
}

#[test]
fn if_stmt() {
    let parsed = parse_stmt(
        r"
        if foo then
            bar
        else
            baz
        end
        ",
    )
    .strip_span();

    let expected = Stmt::If {
        cond: Expr::Ident("foo".to_string()).spanned_default(),
        then_block: vec![
            Stmt::Expr(Expr::Ident("bar".to_string()).spanned_default().into()).spanned_default(),
        ],
        elif_blocks: vec![],
        else_block: Some(vec![Stmt::Expr(
            Expr::Ident("baz".to_string()).spanned_default().into(),
        )
        .spanned_default()]),
    };

    assert_eq!(parsed, expected)
}

#[test]
fn for_loop_stmt() {
    let parsed = parse_stmt(
        r"
        for i = 0, 10 do
            foo
        end
        ",
    )
    .strip_span();

    let expected = Stmt::For {
        init: "i".to_string(),
        from: Expr::Literal(Literal::Int(0)).spanned_default(),
        to: Expr::Literal(Literal::Int(10)).spanned_default(),
        body: vec![
            Stmt::Expr(Expr::Ident("foo".to_string()).spanned_default().into()).spanned_default(),
        ],
    };

    assert_eq!(parsed, expected)
}

#[test]
fn while_loop_stmt() {
    let parsed = parse_stmt(
        r"
        while foo do
            bar
        end
        ",
    )
    .strip_span();

    let expected = Stmt::While {
        cond: Expr::Ident("foo".to_string()).spanned_default(),
        body: vec![
            Stmt::Expr(Expr::Ident("bar".to_string()).spanned_default().into()).spanned_default(),
        ],
    };

    assert_eq!(parsed, expected)
}

#[test]
fn import_stmt() {
    let parsed = parser("import foo.bar.baz")
        .parse_toplevel_stmt()
        .expect("Failed to parse import statement");

    println!("import_stmt: {:#?}", parsed)
}

#[test]
fn return_stmt() {
    let parsed = parse_stmt("return 1 + 2").strip_span();

    let expected = Stmt::Return(Some(
        Expr::BinOp(
            Box::new(Expr::Literal(Literal::Int(1)).spanned_default()),
            Op::Add,
            Box::new(Expr::Literal(Literal::Int(2)).spanned_default()),
        )
        .spanned_default(),
    ));

    assert_eq!(parsed, expected)
}
