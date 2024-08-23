//! Middle-end of the compiler.
//!
//! Multiple passes are performed here in order to transform the AST
//! 1. Type checking and type inference
//! 2. Name resolution
//! 3. Building a module from a file

pub mod module;
pub mod resolver;
pub mod sema_error;
pub mod type_infer;
pub mod typecheck;
pub mod typed_ast;
