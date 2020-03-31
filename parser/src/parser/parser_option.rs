use crate::Parser;

use ast::prelude::*;
pub use errs::Error;
use errs::prelude::*;

impl<'a> Parser<'a> {
    ///Helper function to create a 'null' expr
    pub(crate) fn create_null(
        _loc: &SourceLocation,
    ) -> TypedExpr {
        panic!()
    }
}