use core::fmt::Display;
use std::sync::Arc;

use super::Expression;

use parse_display::DisplayFormat;

pub(crate) struct FunctionCallMember;

impl DisplayFormat<Option<Box<Expression>>> for FunctionCallMember {
    fn write(
        &self,
        f: &mut std::fmt::Formatter,
        value: &Option<Box<Expression>>,
    ) -> std::fmt::Result {
        if let Some(value) = value {
            write!(f, "{value}.")
        } else {
            Ok(())
        }
    }
}

pub(crate) struct Csv;

impl<T: Display> DisplayFormat<Vec<T>> for Csv {
    fn write(&self, f: &mut std::fmt::Formatter, value: &Vec<T>) -> std::fmt::Result {
        for (i, e) in value.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{e}")?;
        }
        Ok(())
    }
}

pub(crate) struct Map;

impl<K: Display, V: Display> DisplayFormat<Vec<(K, V)>> for Map {
    fn write(&self, f: &mut std::fmt::Formatter, value: &Vec<(K, V)>) -> std::fmt::Result {
        for (i, (k, v)) in value.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{k}: {v}")?;
        }
        Ok(())
    }
}

pub(crate) struct StringAtom;

impl DisplayFormat<Arc<String>> for StringAtom {
    fn write(&self, f: &mut std::fmt::Formatter, value: &Arc<String>) -> std::fmt::Result {
        let value = value
            .replace('"', r#"\""#)
            .escape_unicode()
            .to_string()
            .replace('"', r#"\""#);
        write!(f, "\"{value}\"")
    }
}

pub(crate) struct BytesAtom;

impl DisplayFormat<Arc<Vec<u8>>> for BytesAtom {
    fn write(&self, f: &mut std::fmt::Formatter, value: &Arc<Vec<u8>>) -> std::fmt::Result {
        let value = value.escape_ascii().to_string().replace('"', r#"\""#);
        write!(f, "b\"{value}\"")
    }
}
