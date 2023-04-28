use crate::objects::CelType;
use crate::{functions, ExecutionError};
use cel_parser::Expression;
use std::collections::HashMap;

/// A function that can be loaded into the [`Context`] and then called from a Common Expression
/// Language program. If the function is a method, the first parameter will be the target object.
/// If the function accepts arguments, they will be provided as expressions.
type ContextFunction =
    dyn Fn(Option<&CelType>, &[Expression], &Context) -> Result<CelType, ExecutionError>;

pub struct Context {
    pub variables: HashMap<String, CelType>,
    pub functions: HashMap<String, Box<ContextFunction>>,
}

impl Context {
    /// Adds a named variable to the context. This variable is accessible with the scope
    /// of the program being executed.
    pub fn add_variable<S, V>(&mut self, name: S, value: V)
    where
        V: Into<CelType>,
        S: Into<String>,
    {
        self.variables.insert(name.into(), value.into());
    }

    pub fn add_function<S, F: 'static>(&mut self, name: S, value: F)
    where
        F: Fn(Option<&CelType>, &[Expression], &Context) -> Result<CelType, ExecutionError>,
        S: Into<String>,
    {
        self.functions.insert(name.into(), Box::new(value));
    }
}

impl Default for Context {
    fn default() -> Self {
        let mut ctx = Context {
            variables: Default::default(),
            functions: Default::default(),
        };
        ctx.add_function("contains", functions::contains);
        ctx.add_function("intoSet", functions::into_set);
        ctx.add_function("size", functions::size);
        ctx.add_function("has", functions::has);
        ctx
    }
}
