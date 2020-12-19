use std::collections::HashMap;
use crate::objects::CelType;
use crate::ast::Expression;
use crate::functions;

pub struct Context<'a> {
    pub variables: HashMap<&'a str, CelType<'a>>,
    pub functions: HashMap<&'a str, Box<dyn Fn(Option<&'a CelType<'a>>, &[Expression], &'a Context<'a>) -> CelType<'a>>>,
}


impl<'a> Context<'a> {
    pub fn add_variable(&mut self, name: &'a str, value: CelType<'a>) {
        self.variables.insert(name, value);
    }

    pub fn add_function<F: 'static + 'a>(&mut self, name: &'a str, value: F) where F: Fn(Option<&'a CelType<'a>>, &[Expression], &'a Context<'a>) -> CelType<'a> {
        self.functions.insert(name, Box::new(value));
    }
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        let mut ctx = Context {
            variables: Default::default(),
            functions: Default::default(),
        };

        ctx.add_function("size", |target, expr, context| functions::size(target, expr, context));

        ctx
    }
}