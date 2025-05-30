use crate::ast::{Expr, IdedExpr};
use crate::gen::{
    BoolFalseContext, BoolTrueContext, CalcContext, CalcContextAttrs, ConditionalAndContext,
    ConditionalOrContext, ConstantLiteralContext, ConstantLiteralContextAttrs, ExprContext,
    IntContext, MemberExprContext, MemberExprContextAttrs, PrimaryExprContext,
    PrimaryExprContextAttrs, RelationContext, RelationContextAttrs, StartContext,
    StartContextAttrs, StringContext, UintContext,
};
use crate::reference::Val;
use crate::{ast, gen};
use antlr4rust::common_token_stream::CommonTokenStream;
use antlr4rust::parser::ParserNodeType;
use antlr4rust::token::Token;
use antlr4rust::tree::{ParseTree, ParseTreeVisitorCompat, VisitChildren};
use antlr4rust::InputStream;
use std::mem;
use std::ops::Deref;

#[derive(Debug)]
pub struct ParserError {}

pub struct Parser {
    ast: ast::Ast,
    helper: ParserHelper,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            ast: ast::Ast {
                expr: IdedExpr::default(),
            },
            helper: ParserHelper::default(),
        }
    }

    pub fn parse(mut self, source: &str) -> Result<IdedExpr, ParserError> {
        let stream = InputStream::new(source);
        let mut lexer = gen::CELLexer::new(stream);
        lexer.remove_error_listeners();
        // lexer.add_error_listener()

        let mut prsr = gen::CELParser::new(CommonTokenStream::new(lexer));
        match prsr.start() {
            Ok(t) => Ok(self.visit(t.deref())),
            Err(_) => Err(ParserError {}),
        }
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl ParseTreeVisitorCompat<'_> for Parser {
    type Node = gen::CELParserContextType;
    type Return = IdedExpr;
    fn temp_result(&mut self) -> &mut Self::Return {
        &mut self.ast.expr
    }

    fn visit(&mut self, node: &<Self::Node as ParserNodeType<'_>>::Type) -> Self::Return {
        //println!("{node:?}");
        self.visit_node(node);
        mem::take(self.temp_result())
    }

    fn aggregate_results(&self, _aggregate: Self::Return, next: Self::Return) -> Self::Return {
        next
    }
}

impl gen::CELVisitorCompat<'_> for Parser {
    fn visit_start(&mut self, ctx: &StartContext<'_>) -> Self::Return {
        self.visit(ctx.expr().as_deref().unwrap())
    }

    fn visit_expr(&mut self, ctx: &ExprContext<'_>) -> Self::Return {
        if ctx.op.is_none() {
            <Self as ParseTreeVisitorCompat>::visit(self, ctx.e.as_deref().unwrap())
        } else {
            <Self as ParseTreeVisitorCompat>::visit_children(self, ctx)
        }
    }

    fn visit_conditionalOr(&mut self, ctx: &ConditionalOrContext<'_>) -> Self::Return {
        if ctx.ops.is_empty() {
            <Self as ParseTreeVisitorCompat>::visit(self, ctx.e.as_deref().unwrap())
        } else {
            <Self as ParseTreeVisitorCompat>::visit_children(self, ctx)
        }
    }

    fn visit_conditionalAnd(&mut self, ctx: &ConditionalAndContext<'_>) -> Self::Return {
        if ctx.ops.is_empty() {
            <Self as ParseTreeVisitorCompat>::visit(self, ctx.e.as_deref().unwrap())
        } else {
            <Self as ParseTreeVisitorCompat>::visit_children(self, ctx)
        }
    }

    fn visit_relation(&mut self, ctx: &RelationContext<'_>) -> Self::Return {
        if ctx.op.is_none() {
            <Self as ParseTreeVisitorCompat>::visit(self, ctx.calc().as_deref().unwrap())
        } else {
            <Self as ParseTreeVisitorCompat>::visit_children(self, ctx)
        }
    }

    fn visit_calc(&mut self, ctx: &CalcContext<'_>) -> Self::Return {
        match &ctx.op {
            None => self.visit(ctx.unary().as_deref().unwrap()),
            Some(op) => {
                let _lhs = self.visit(ctx.calc(0).unwrap().deref());
                let _rhs = self.visit(ctx.calc(1).unwrap().deref());
                println!(
                    "{} {} {}",
                    ctx.calc(0).unwrap().deref().get_text(),
                    op.get_text(),
                    ctx.calc(1).unwrap().deref().get_text()
                );
                self.helper.next_expr(Expr::Call)
            }
        }
    }

    fn visit_MemberExpr(&mut self, ctx: &MemberExprContext<'_>) -> Self::Return {
        <Self as ParseTreeVisitorCompat>::visit(self, ctx.member().as_deref().unwrap())
    }

    fn visit_PrimaryExpr(&mut self, ctx: &PrimaryExprContext<'_>) -> Self::Return {
        <Self as ParseTreeVisitorCompat>::visit(self, ctx.primary().as_deref().unwrap())
    }

    fn visit_ConstantLiteral(&mut self, ctx: &ConstantLiteralContext<'_>) -> Self::Return {
        <Self as ParseTreeVisitorCompat>::visit(self, ctx.literal().as_deref().unwrap())
    }

    fn visit_Int(&mut self, ctx: &IntContext<'_>) -> Self::Return {
        let string = ctx.get_text();
        let val = if let Some(string) = string.strip_prefix("0x") {
            i64::from_str_radix(string, 16)
        } else {
            string.parse::<i64>()
        }
        .unwrap();
        self.helper.next_expr(Expr::Literal(Val::Int(val)))
    }

    fn visit_Uint(&mut self, ctx: &UintContext<'_>) -> Self::Return {
        let mut string = ctx.get_text();
        string.truncate(string.len() - 1);
        let val = if let Some(string) = string.strip_prefix("0x") {
            u64::from_str_radix(string, 16)
        } else {
            string.parse::<u64>()
        }
        .unwrap();
        self.helper.next_expr(Expr::Literal(Val::UInt(val)))
    }

    fn visit_String(&mut self, ctx: &StringContext<'_>) -> Self::Return {
        self.helper
            .next_expr(Expr::Literal(Val::String(ctx.get_text())))
    }

    fn visit_BoolTrue(&mut self, _ctx: &BoolTrueContext<'_>) -> Self::Return {
        self.helper.next_expr(Expr::Literal(Val::Boolean(true)))
    }

    fn visit_BoolFalse(&mut self, _ctx: &BoolFalseContext<'_>) -> Self::Return {
        self.helper.next_expr(Expr::Literal(Val::Boolean(false)))
    }
}

pub struct ParserHelper {
    next_id: u64,
}

impl Default for ParserHelper {
    fn default() -> Self {
        Self { next_id: 1 }
    }
}

impl ParserHelper {
    fn next_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    pub fn next_expr(&mut self, expr: Expr) -> IdedExpr {
        IdedExpr {
            id: self.next_id(),
            expr,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;
    use crate::reference::Val;

    struct TestInfo {
        // I contains the input expression to be parsed.
        i: &'static str,

        // P contains the type/id adorned debug output of the expression tree.
        p: &'static str,
        // E contains the expected error output for a failed parse, or "" if the parse is expected to be successful.
        // e: String,

        // L contains the expected source adorned debug output of the expression tree.
        // l: String,

        // M contains the expected adorned debug output of the macro calls map
        // m: String,

        // Opts contains the list of options to be configured with the parser before parsing the expression.
        // Opts []Option
    }
    #[test]
    fn test() {
        let test_cases: [TestInfo; 11] = [
            TestInfo {
                i: r#""A""#,
                p: r#""A"^#1:*expr.Constant_StringValue#"#,
            },
            TestInfo {
                i: r#"true"#,
                p: r#"true^#1:*expr.Constant_BoolValue#"#,
            },
            TestInfo {
                i: r#"false"#,
                p: r#"false^#1:*expr.Constant_BoolValue#"#,
            },
            TestInfo {
                i: "0",
                p: "0^#1:*expr.Constant_Int64Value#",
            },
            TestInfo {
                i: "42",
                p: "42^#1:*expr.Constant_Int64Value#",
            },
            TestInfo {
                i: "0xF",
                p: "15^#1:*expr.Constant_Int64Value#",
            },
            TestInfo {
                i: "0u",
                p: "0u^#1:*expr.Constant_Uint64Value#",
            },
            TestInfo {
                i: "23u",
                p: "23u^#1:*expr.Constant_Uint64Value#",
            },
            TestInfo {
                i: "24u",
                p: "24u^#1:*expr.Constant_Uint64Value#",
            },
            TestInfo {
                i: "0xFu",
                p: "15u^#1:*expr.Constant_Uint64Value#",
            },
            TestInfo {
                i: "-1",
                p: "-1^#1:*expr.Constant_Int64Value#",
            },
        ];

        for test_case in test_cases {
            let parser = Parser::new();
            let result = parser.parse(&test_case.i);
            assert_eq!(
                to_go_like_string(result.unwrap().expr),
                test_case.p,
                "Expr `{}` failed",
                test_case.i
            );
        }
    }

    fn to_go_like_string(expr: Expr) -> String {
        match expr {
            Expr::Unspecified => "*expr.Unspecified".to_string(),
            Expr::Call => "*expr.Expr_CallExpr".to_string(),
            Expr::Comprehension => "".to_string(),
            Expr::Ident => "".to_string(),
            Expr::List => "".to_string(),
            Expr::Literal(v) => match v {
                Val::String(s) => {
                    format!("{s}^#{}:{}#", 1, "*expr.Constant_StringValue")
                }
                Val::Boolean(b) => {
                    format!("{b}^#{}:{}#", 1, "*expr.Constant_BoolValue")
                }
                Val::Int(i) => {
                    format!("{i}^#{}:{}#", 1, "*expr.Constant_Int64Value")
                }
                Val::UInt(u) => {
                    format!("{u}u^#{}:{}#", 1, "*expr.Constant_Uint64Value")
                }
            },
            Expr::Map => "".to_string(),
            Expr::Select => "".to_string(),
            Expr::Struct => "".to_string(),
        }
    }
}
