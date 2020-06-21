use crate::Parser;

use wa1_ast::prelude::*;
pub use wa1_errs::Error;
use wa1_errs::prelude::*;

impl<'a> Parser<'a> {
    ///Helper function to create a 'null' expr
    pub(crate) fn create_null(
        _loc: &SourceLocation,
    ) -> TypedExpr {
        panic!()
    }
}