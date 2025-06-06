use std::collections::HashSet;

use crate::ast::{Expr, IdedExpr};

/// A collection of all the references that an expression makes to variables and functions.
pub struct ExpressionReferences<'expr> {
    variables: HashSet<&'expr str>,
    functions: HashSet<&'expr str>,
}

impl<'expr> ExpressionReferences<'expr> {
    /// Returns true if the expression references the provided variable name.
    ///
    /// # Example
    /// ```rust
    /// # use cel_antlr::parse;
    /// let expression = parse("foo.bar == true").unwrap();
    /// let references = expression.references();
    /// assert!(references.has_variable("foo"));
    /// ```
    pub fn has_variable(&self, name: impl AsRef<str>) -> bool {
        self.variables.contains(name.as_ref())
    }

    /// Returns true if the expression references the provided function name.
    ///
    /// # Example
    /// ```rust
    /// # use cel_antlr::parse;
    /// let expression = parse("size(foo) > 0").unwrap();
    /// let references = expression.references();
    /// assert!(references.has_function("size"));
    /// ```
    pub fn has_function(&self, name: impl AsRef<str>) -> bool {
        self.functions.contains(name.as_ref())
    }

    /// Returns a list of all variables referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_antlr::parse;
    /// let expression = parse("foo.bar == true").unwrap();
    /// let references = expression.references();
    /// assert_eq!(vec!["foo"], references.variables());
    /// ```
    pub fn variables(&self) -> Vec<&str> {
        self.variables.iter().copied().collect()
    }

    /// Returns a list of all functions referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_antlr::parse;
    /// let expression = parse("size(foo) > 0").unwrap();
    /// let references = expression.references();
    /// assert_eq!(vec!["size"], references.functions());
    /// ```
    pub fn functions(&self) -> Vec<&str> {
        self.functions.iter().copied().collect()
    }
}

impl IdedExpr {
    /// Returns a set of all variables and functions referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_antlr::parse;
    /// let expression = parse("foo && size(foo) > 0").unwrap();
    /// let references = expression.references();
    ///
    /// assert!(references.has_variable("foo"));
    /// assert!(references.has_function("size"));
    /// ```
    pub fn references(&self) -> ExpressionReferences {
        let mut variables = HashSet::new();
        let mut functions = HashSet::new();
        self._references(&mut variables, &mut functions);
        ExpressionReferences {
            variables,
            functions,
        }
    }

    /// Internal recursive function to collect all variable and function references in the expression.
    fn _references<'expr>(
        &'expr self,
        variables: &mut HashSet<&'expr str>,
        functions: &mut HashSet<&'expr str>,
    ) {
        match &self.expr {
            Expr::Unspecified => {}
            Expr::Call(call) => {
                functions.insert(&call.func_name);
                if let Some(target) = &call.target {
                    target._references(variables, functions);
                }
                for arg in &call.args {
                    arg._references(variables, functions);
                }
            }
            Expr::Comprehension(comp) => {
                comp.iter_range._references(variables, functions);
                comp.accu_init._references(variables, functions);
                comp.loop_cond._references(variables, functions);
                comp.loop_step._references(variables, functions);
                comp.result._references(variables, functions);
            }
            Expr::Ident(name) => {
                variables.insert(name);
            }
            Expr::List(list) => {
                for elem in &list.elements {
                    elem._references(variables, functions);
                }
            }
            Expr::Literal(_) => {}
            Expr::Map(map) => {
                for entry in &map.entries {
                    match &entry.expr {
                        crate::ast::EntryExpr::StructField(field) => {
                            field.value._references(variables, functions);
                        }
                        crate::ast::EntryExpr::MapEntry(map_entry) => {
                            map_entry.key._references(variables, functions);
                            map_entry.value._references(variables, functions);
                        }
                    }
                }
            }
            Expr::Select(select) => {
                select.operand._references(variables, functions);
            }
            Expr::Struct(struct_expr) => {
                for entry in &struct_expr.entries {
                    match &entry.expr {
                        crate::ast::EntryExpr::StructField(field) => {
                            field.value._references(variables, functions);
                        }
                        crate::ast::EntryExpr::MapEntry(map_entry) => {
                            map_entry.key._references(variables, functions);
                            map_entry.value._references(variables, functions);
                        }
                    }
                }
            }
        }
    }
}
