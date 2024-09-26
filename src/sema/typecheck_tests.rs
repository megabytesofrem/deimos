#![cfg(test)]

use crate::{
    sema::{resolver::Resolver, typecheck::Typechecker},
    syntax::ast_types::{SizedNumber, StructureInfo, StructureKind, Ty},

    syntax::lexer::SourceLoc,

    // Import the parse_expr and parse_stmt functions from parser_tests
    syntax::parser::parser_tests::{parse_expr, parse_stmt},
};

fn typechecker() -> Typechecker {
    Typechecker::new(Resolver::new("test_suite"))
}

// Typechecking tests
#[test]
fn simple_expr_typechecks() {
    let tc = typechecker();
    let expr = parse_expr("1 + 2");
    let ty = tc.infer_expr(&expr.spanned_default()).unwrap();

    assert_eq!(ty, Ty::Number(SizedNumber::I32));
}

#[test]
fn member_access_typechecks() {
    let tc = typechecker();
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

// Unification tests
#[test]
fn can_unify_identical_types() {
    let tc = typechecker();
    let ty1 = Ty::Number(SizedNumber::I32);
    let ty2 = Ty::Number(SizedNumber::I32);
    let loc = SourceLoc::default();

    assert!(tc.unify(&ty1, &ty2, &loc).is_ok())
}

#[test]
fn cannot_unify_different_types() {
    let tc = typechecker();
    let ty1 = Ty::Number(SizedNumber::I32);
    let ty2 = Ty::Number(SizedNumber::F32);
    let loc = SourceLoc::default();

    assert!(tc.unify(&ty1, &ty2, &loc).is_err())
}

#[test]
fn can_unify_structs_same_fields() {
    let tc = typechecker();
    let struct_a = Ty::Struct(StructureInfo {
        kind: StructureKind::Struct,
        name: "StructA".to_string(),
        fields: vec![
            ("foo".to_string(), Ty::Number(SizedNumber::I32)),
            ("bar".to_string(), Ty::Number(SizedNumber::F32)),
        ],
    });

    let struct_b = Ty::Struct(StructureInfo {
        kind: StructureKind::Struct,
        name: "StructB".to_string(),
        fields: vec![
            ("foo".to_string(), Ty::Number(SizedNumber::I32)),
            ("bar".to_string(), Ty::Number(SizedNumber::F32)),
        ],
    });

    let loc = SourceLoc::default();
    assert!(tc.unify(&struct_a, &struct_b, &loc).is_ok())
}

#[test]
fn can_unify_array_same_types() {
    let tc = typechecker();
    let array_a = Ty::Array(Box::new(Ty::Number(SizedNumber::I32)));
    let array_b = Ty::Array(Box::new(Ty::Number(SizedNumber::I32)));
    let loc = SourceLoc::default();

    assert!(tc.unify(&array_a, &array_b, &loc).is_ok())
}

#[test]
fn cannot_unify_different_user_types() {
    let tc = typechecker();
    let ty1 = Ty::UserDefined("User1".to_string());
    let ty2 = Ty::UserDefined("User2".to_string());
    let loc = SourceLoc::default();

    assert!(tc.unify(&ty1, &ty2, &loc).is_err())
}
