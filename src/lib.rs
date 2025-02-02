#![deny(clippy::all)]
#![allow(clippy::comparison_chain, clippy::too_many_arguments)]

pub mod compiler;
pub mod sbnf;
pub mod sublime_syntax;
mod text_location;
mod symbol_table;
mod util;

pub use text_location::{TextLocation, TextLocationWithSource};
