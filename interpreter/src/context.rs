use crate::functions;
use crate::objects::CelType;
use cel_parser::Expression;
use std::collections::HashMap;

/// A function that can be loaded into the [`Context`] and then called from a Common Expression
/// Language program. If the function is a method, the first parameter will be the target object.
/// If the function accepts arguments, they will be provided as expressions.
type ContextFunction = dyn Fn(Option<&CelType>, &[Expression], &Context) -> functions::Result;

pub struct Context {
    pub variables: HashMap<String, CelType>,
    pub functions: HashMap<String, Box<ContextFunction>>,
}

impl Context {
    pub fn add_variable<S, V>(&mut self, name: S, value: V)
    where
        V: Into<CelType>,
        S: Into<String>,
    {
        self.variables.insert(name.into(), value.into());
    }

    pub fn add_function<F: 'static>(&mut self, name: String, value: F)
    where
        F: Fn(Option<&CelType>, &[Expression], &Context) -> functions::Result,
    {
        self.functions.insert(name, Box::new(value));
    }
}

impl Default for Context {
    fn default() -> Self {
        let mut ctx = Context {
            variables: Default::default(),
            functions: Default::default(),
        };

        ctx.add_function("size".into(), |target, expr, context| {
            functions::size(target, expr, context)
        });
        ctx.add_function("has".into(), |target, expr, context| {
            functions::has(target, expr, context)
        });

        ctx
    }
}
