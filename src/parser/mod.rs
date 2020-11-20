type Result<T> = std::result::Result<T, SchemeError>;

#[macro_use]
pub mod pair;

mod lexer;
pub use lexer::*;
mod macros;
pub use macros::*;
mod parser;
pub use parser::*;
mod datum;
pub use datum::*;

use crate::error::SchemeError;
pub mod error;
