use std::{fmt::Display, iter};

use lalrpop_util::lexer::Token;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub absolute: usize,
}

#[derive(Debug, Default)]
pub struct Span {
    pub start: Option<Location>,
    pub end: Option<Location>,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (&self.start, &self.end) {
            (Some(start), Some(end)) if start == end => {
                write!(f, "[{}:{}]", start.line, start.column)
            }
            (Some(location), None) | (None, Some(location)) => {
                write!(f, "[{}:{}]", location.line, location.column)
            }
            (Some(start), Some(end)) => write!(
                f,
                "[{}:{}]->[{}:{}]",
                start.line, start.column, end.line, end.column
            ),
            (None, None) => write!(f, "?",),
        }
    }
}

impl Span {
    fn single_location(location: Location) -> Span {
        Span {
            start: Some(location.clone()),
            end: Some(location),
        }
    }
}

#[derive(Error, Debug)]
#[error("Error parsing: {msg} at {span}")]
pub struct ParseError {
    pub msg: String,
    pub expected: Vec<String>,
    pub span: Span,
}

impl ParseError {
    pub(super) fn from_lalrpop(
        src_str: &str,
        err: lalrpop_util::ParseError<usize, Token<'_>, &str>,
    ) -> Self {
        use lalrpop_util::ParseError::*;

        match err {
            InvalidToken { location } => ParseError {
                span: byte_pos_to_src_location(src_str, location)
                    .map(Span::single_location)
                    .unwrap_or_default(),
                expected: Vec::new(),
                msg: "invalid token".into(),
            },
            UnrecognizedEof { location, expected } => ParseError {
                msg: "unrecognized eof".into(),
                span: byte_pos_to_src_location(src_str, location)
                    .map(Span::single_location)
                    .unwrap_or_default(),
                expected,
            },
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => ParseError {
                msg: format!("unrecognized token: '{}'", token),
                span: Span {
                    start: byte_pos_to_src_location(src_str, start),
                    end: byte_pos_to_src_location(src_str, end),
                },
                expected,
            },
            ExtraToken {
                token: (start, token, end),
            } => ParseError {
                msg: format!("extra token: '{}'", token),
                span: Span {
                    start: byte_pos_to_src_location(src_str, start),
                    end: byte_pos_to_src_location(src_str, end),
                },
                expected: Vec::new(),
            },
            User { error } => ParseError {
                msg: error.into(),
                expected: Vec::new(),
                span: Span::default(),
            },
        }
    }
}

// Slightly simplified but heavily inspired by
// https://github.com/gluon-lang/gluon/blob/f8326d21a14b5f21d203e9c43fa5bb7f0688a74c/base/src/source.rs
fn byte_pos_to_src_location(src_str: &str, byte_pos: usize) -> Option<Location> {
    let src_bytes = src_str.as_bytes();
    let total_len = src_bytes.len();

    let line_indices: Vec<usize> = {
        let input_indices = src_bytes
            .iter()
            .enumerate()
            .filter(|&(_, b)| *b == b'\n')
            .map(|(i, _)| i + 1); // index of first char in the line

        iter::once(0).chain(input_indices).collect()
    };

    if byte_pos <= total_len {
        let num_lines = line_indices.len();

        let line_index = (0..num_lines)
            .find(|&i| line_indices[i] > byte_pos)
            .map(|i| i - 1)
            .unwrap_or(num_lines - 1);

        let line_byte_pos = line_indices[line_index];
        Some(Location {
            line: line_index,
            column: byte_pos - line_byte_pos,
            absolute: byte_pos,
        })
    } else {
        None
    }
}
