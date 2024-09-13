#![cfg(test)]

use crate::{
    sema::{resolver::Resolver, typecheck::Typechecker},
    syntax::ast_types::{SizedNumber, StructureInfo, StructureKind, Ty},

    // Import the parse_expr and parse_stmt functions from parser_tests
    tests::parser_tests::{parse_expr, parse_stmt},
};

fn typechecker() -> Typechecker {
    Typechecker::new(Resolver::new("test_suite"))
}

// Typechecking tests
#[test]
fn simple_expr_typechecks() {
    let mut tc = typechecker();
    let expr = parse_expr("1 + 2");
    let ty = tc.infer_expr(&expr.spanned_default()).unwrap();

    assert_eq!(ty, Ty::Number(SizedNumber::I32));
}

#[test]
fn member_access_typechecks() {
    let mut tc = typechecker();
    tc.resolver
        .borrow_mut()
        .insert_name(
            "x",
            Ty::Struct(StructureInfo {
                kind: StructureKind::Struct,
                name: "_test_suite".to_string(),
                fields: vec![("y".to_string(), Ty::Number(SizedNumber::I32))],
            }),
        )
        .unwrap();

    let expr = parse_expr("x.y");
    let ty = tc.infer_expr(&expr.spanned_default()).unwrap();

    assert_eq!(ty, Ty::Number(SizedNumber::I32));
}

#[test]
fn let_stmt_typechecks() {
    let mut tc = typechecker();
    let stmt = parse_stmt("let x: i32 = 1");
    let tstmt = tc.check_stmt(&stmt.spanned_default());

    assert!(tstmt.is_ok());
}

#[test]
fn assign_stmt_typechecks() {
    let mut tc = typechecker();
    tc.resolver
        .borrow_mut()
        .insert_name("x", Ty::Number(SizedNumber::I32))
        .unwrap();

    let stmt = parse_stmt("x = 1 + 2");
    let tstmt = tc.check_stmt(&stmt.spanned_default());

    assert!(tstmt.is_ok());
}
