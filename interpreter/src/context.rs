use crate::objects::CelType;
use crate::{functions, ExecutionError};
use cel_parser::Expression;
use std::collections::HashMap;

/// A function that can be loaded into the [`ParentContext`] and then called from a Common Expression
/// Language program. If the function is a method, the first parameter will be the target object.
/// If the function accepts arguments, they will be provided as expressions.
type ContextFunction =
    dyn Fn(Option<&CelType>, &[Expression], &Context) -> Result<CelType, ExecutionError>;

/// Context is a collection of variables and functions that can be used
/// by the interpreter to resolve expressions. The context can be either
/// a parent context, or a child context. A parent context is created by
/// default and contains all of the built-in functions. A child context
/// can be created by calling `.clone()`. The child context has it's own
/// variables (which can be added to), but it will also reference the
/// parent context. This allows for variables to be overridden within the
/// child context while still being able to resolve variables in the child's
/// parents. You can have theoretically have an infinite number of child
/// contexts that reference each-other.
///
/// So why is this important? Well some CEL-macros such as the `.map` macro
/// declare intermediate user-specified identifiers that should only be
/// available within the macro, and should not override variables in the
/// parent context. The `.map` macro can clone the parent context, add the
/// intermediate identifier to the child context, and then evaluate the
/// map expression.
///
/// Intermediate variable stored in child context
///               ↓
/// [1, 2, 3].map(x, x * 2) == [2, 4, 6]
///                  ↑
/// Only in scope for the duration of the map expression
///
pub enum Context<'a> {
    Root {
        functions: HashMap<String, Box<ContextFunction>>,
        variables: HashMap<String, CelType>,
    },
    Child {
        parent: &'a Context<'a>,
        variables: HashMap<String, CelType>,
    },
}

impl<'a> Context<'a> {
    pub fn add_variable<S, V>(&mut self, name: S, value: V)
    where
        S: Into<String>,
        V: Into<CelType>,
    {
        match self {
            Context::Root { variables, .. } => {
                variables.insert(name.into(), value.into());
            }
            Context::Child { variables, .. } => {
                variables.insert(name.into(), value.into());
            }
        }
    }

    pub fn get_variable<S>(&self, name: S) -> Result<CelType, ExecutionError>
    where
        S: Into<String>,
    {
        let name = name.into();
        match self {
            Context::Child { variables, parent } => variables
                .get(&name)
                .cloned()
                .or_else(|| parent.get_variable(&name).ok())
                .ok_or(ExecutionError::UndeclaredReference(name.into())),
            Context::Root { variables, .. } => variables
                .get(&name)
                .cloned()
                .ok_or(ExecutionError::UndeclaredReference(name.into())),
        }
    }

    pub(crate) fn has_function<S>(&self, name: S) -> bool
    where
        S: Into<String>,
    {
        let name = name.into();
        match self {
            Context::Root { functions, .. } => functions.contains_key(&name),
            Context::Child { parent, .. } => parent.has_function(name),
        }
    }

    pub(crate) fn get_function<S>(&self, name: S) -> Option<&ContextFunction>
    where
        S: Into<String>,
    {
        let name = name.into();
        match self {
            Context::Root { functions, .. } => functions.get(&name).map(|v| v.as_ref()),
            Context::Child { parent, .. } => parent.get_function(name),
        }
    }

    pub fn add_function<S, F: 'static>(&mut self, name: S, value: F)
    where
        F: Fn(Option<&CelType>, &[Expression], &Context) -> Result<CelType, ExecutionError>,
        S: Into<String>,
    {
        if let Context::Root { functions, .. } = self {
            functions.insert(name.into(), Box::new(value));
        };
    }

    pub(crate) fn clone(&self) -> Context {
        Context::Child {
            parent: self,
            variables: Default::default(),
        }
    }
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        let mut ctx = Context::Root {
            variables: Default::default(),
            functions: Default::default(),
        };
        ctx.add_function("contains", functions::contains);
        ctx.add_function("size", functions::size);
        ctx.add_function("has", functions::has);
        ctx.add_function("map", functions::map);
        ctx.add_function("filter", functions::filter);
        ctx.add_function("all", functions::all);
        ctx.add_function("max", functions::max);
        ctx.add_function("duration", functions::duration);
        ctx.add_function("timestamp", functions::timestamp);
        ctx.add_function("startsWith", functions::starts_with);
        ctx.add_function("string", functions::string);
        ctx
    }
}
