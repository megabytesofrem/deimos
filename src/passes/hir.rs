/// HIR transformation pass
///
/// This pass transforms the AST into the HIR (High-level Intermediate Representation)
/// It contains types, imports, and other high-level constructs
use crate::syntax::ast::{Expr, Program, Stmt, ToplevelStmt, Ty};
use crate::syntax::lexer::{BinOp, UnOp};

#[derive(Debug, Clone)]
pub enum HirExpr {
    // Primitive types
    Literal(Expr),
    Variable(String),

    Array(Vec<HirExpr>),
    Tuple(Vec<HirExpr>),

    // Struct constructor
    StructCons {
        fields: Vec<(String, HirExpr)>,
    },

    // Operations
    BinOp(Box<HirExpr>, BinOp, Box<HirExpr>),
    UnOp(UnOp, Box<HirExpr>),
    ArrayIndex {
        array: Box<HirExpr>,
        index: Box<HirExpr>,
    },
    Call {
        func: Box<HirExpr>,
        args: Vec<HirExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum HirStmt {
    Expr(HirExpr),
    Return(Option<HirExpr>),

    Local {
        name: String,
        ty: Option<Ty>,
        value: Option<HirExpr>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },
    Assign {
        target: HirExpr,
        value: HirExpr,
    },
    If {
        cond: HirExpr,
        then_block: HirBlock,
        else_block: Option<HirBlock>,
    },
    For {
        init: String,
        from: HirExpr,
        to: HirExpr,
        body: HirBlock,
    },
    While {
        cond: HirExpr,
        block: HirBlock,
    },
}

pub type HirBlock = Vec<HirStmt>;
pub type HirProgram = Vec<HirToplevelStmt>;

#[derive(Debug, Clone)]
pub enum HirToplevelStmt {
    Stmt(HirStmt),
    ToplevelStmt(ToplevelStmt),
}

// Lowering functions

fn lower_expr(expr: &Expr) -> HirExpr {
    match expr {
        Expr::Int(..) | Expr::Float(..) | Expr::Double(..) | Expr::Bool(..) | Expr::String(..) => {
            HirExpr::Literal(expr.clone())
        }
        Expr::Variable(v) => HirExpr::Variable(v.clone()),
        Expr::Array(elems) => HirExpr::Array(elems.iter().map(lower_expr).collect()),
        _ => unimplemented!(),
    }
}

fn lower_bin_op(lhs: &Expr, op: BinOp, rhs: &Expr) -> HirExpr {
    HirExpr::BinOp(Box::new(lower_expr(lhs)), op, Box::new(lower_expr(rhs)))
}

fn lower_un_op(op: UnOp, expr: &Expr) -> HirExpr {
    HirExpr::UnOp(op, Box::new(lower_expr(expr)))
}

fn lower_stmt(stmt: &Stmt) -> HirStmt {
    match stmt {
        Stmt::Expr(expr) => HirStmt::Expr(lower_expr(expr)),
        Stmt::Return(expr) => HirStmt::Return(expr.as_ref().map(lower_expr)),

        Stmt::Local { name, ty, value } => HirStmt::Local {
            name: name.clone(),
            ty: ty.clone(),
            value: value.as_ref().map(|e| lower_expr(&e.raw)),
        },
        Stmt::StructDecl { name, fields } => HirStmt::StructDecl {
            name: name.clone(),
            fields: fields.clone(),
        },
        Stmt::Assign { target, value } => HirStmt::Assign {
            target: lower_expr(target),
            value: lower_expr(&value.raw),
        },
        Stmt::If {
            cond,
            then_block,
            else_block,
        } => HirStmt::If {
            cond: lower_expr(&cond.raw),
            then_block: lower_block(then_block),
            else_block: else_block.as_ref().map(lower_block),
        },
        Stmt::For {
            init,
            from,
            to,
            body,
        } => HirStmt::For {
            init: init.clone(),
            from: lower_expr(&from.raw),
            to: lower_expr(&to.raw),
            body: lower_block(body),
        },
        Stmt::While { cond, block } => HirStmt::While {
            cond: lower_expr(cond),
            block: lower_block(block),
        },
    }
}

fn lower_toplevel_stmt(stmt: &ToplevelStmt) -> HirToplevelStmt {
    match stmt {
        ToplevelStmt::Stmt(stmt) => HirToplevelStmt::Stmt(lower_stmt(stmt)),
        //ToplevelStmt::CImport(s) => HirToplevelStmt::CImport(s.clone()),
        ToplevelStmt::Import { path, alias } => HirToplevelStmt::ToplevelStmt(stmt.clone()),
        ToplevelStmt::FunctionDecl {
            name,
            return_ty,
            params,
            body,
        } => HirToplevelStmt::ToplevelStmt(stmt.clone()),
        _ => unimplemented!(),
    }
}

fn lower_block(block: &Vec<Stmt>) -> HirBlock {
    block.iter().map(lower_stmt).collect()
}

/// Lower the AST into an HIR (High-level Intermediate Representation).
pub fn lower_hir(program: &Program) -> HirProgram {
    program.iter().map(lower_toplevel_stmt).collect()
}
