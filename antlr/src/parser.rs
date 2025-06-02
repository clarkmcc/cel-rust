use crate::ast::{CallExpr, Expr, IdedExpr, SelectExpr};
use crate::gen::{
    BoolFalseContext, BoolTrueContext, BytesContext, CalcContext, CalcContextAttrs,
    ConditionalAndContext, ConditionalOrContext, ConstantLiteralContext,
    ConstantLiteralContextAttrs, DoubleContext, ExprContext, IdentContext, IntContext,
    LogicalNotContext, LogicalNotContextAttrs, MemberCallContext, MemberCallContextAttrs,
    MemberExprContext, MemberExprContextAttrs, NestedContext, NestedContextAttrs, NullContext,
    PrimaryExprContext, PrimaryExprContextAttrs, RelationContext, RelationContextAttrs,
    SelectContext, SelectContextAttrs, StartContext, StartContextAttrs, StringContext, UintContext,
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
    fn new_logic_manager(&self, func: &str, term: IdedExpr) -> LogicManager {
        LogicManager {
            function: func.to_string(),
            terms: vec![term],
            ops: vec![],
        }
    }
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
            let result = self.visit(ctx.e.as_deref().unwrap());
            let op_id = self.helper.next_id();
            let if_true = self.visit(ctx.e1.as_deref().unwrap());
            let if_false = self.visit(ctx.e2.as_deref().unwrap());
            IdedExpr {
                expr: Expr::Call(CallExpr {
                    target: None,
                    func_name: "_?_:_".to_string(),
                    args: vec![result, if_true, if_false],
                }),
                id: op_id,
            }
        }
    }

    fn visit_conditionalOr(&mut self, ctx: &ConditionalOrContext<'_>) -> Self::Return {
        if ctx.ops.is_empty() {
            <Self as ParseTreeVisitorCompat>::visit(self, ctx.e.as_deref().unwrap())
        } else {
            let result = self.visit(ctx.e.as_deref().unwrap());
            let mut l = self.new_logic_manager("_||_", result);
            let rest = &ctx.e1;
            if ctx.ops.len() > rest.len() {
                // why is >= not ok?
                panic!("unexpected character, wanted '||'")
            }
            for (i, _op) in ctx.ops.iter().enumerate() {
                let next = self.visit(rest[i].deref());
                let op_id = self.helper.next_id();
                l.add_term(op_id, next)
            }
            l.expr()
        }
    }

    fn visit_conditionalAnd(&mut self, ctx: &ConditionalAndContext<'_>) -> Self::Return {
        if ctx.ops.is_empty() {
            <Self as ParseTreeVisitorCompat>::visit(self, ctx.e.as_deref().unwrap())
        } else {
            let result = self.visit(ctx.e.as_deref().unwrap());
            let mut l = self.new_logic_manager("_&&_", result);
            let rest = &ctx.e1;
            if ctx.ops.len() > rest.len() {
                // why is >= not ok?
                panic!("unexpected character, wanted '&&'")
            }
            for (i, _op) in ctx.ops.iter().enumerate() {
                let next = self.visit(rest[i].deref());
                let op_id = self.helper.next_id();
                l.add_term(op_id, next)
            }
            l.expr()
        }
    }

    fn visit_relation(&mut self, ctx: &RelationContext<'_>) -> Self::Return {
        if ctx.op.is_none() {
            <Self as ParseTreeVisitorCompat>::visit(self, ctx.calc().as_deref().unwrap())
        } else {
            match &ctx.op {
                None => <Self as ParseTreeVisitorCompat>::visit_children(self, ctx),
                Some(op) => {
                    let lhs = self.visit(ctx.relation(0).unwrap().deref());
                    let op_id = self.helper.next_id();
                    let rhs = self.visit(ctx.relation(1).unwrap().deref());
                    IdedExpr {
                        expr: Expr::Call(CallExpr {
                            target: None,
                            func_name: find_operator(op.get_text())
                                .expect("operator not found!")
                                .to_string(),
                            args: vec![lhs, rhs],
                        }),
                        id: op_id,
                    }
                }
            }
        }
    }

    fn visit_calc(&mut self, ctx: &CalcContext<'_>) -> Self::Return {
        match &ctx.op {
            None => self.visit(ctx.unary().as_deref().unwrap()),
            Some(op) => {
                let lhs = self.visit(ctx.calc(0).unwrap().deref());
                let op_id = self.helper.next_id();
                let rhs = self.visit(ctx.calc(1).unwrap().deref());
                IdedExpr {
                    expr: Expr::Call(CallExpr {
                        target: None,
                        func_name: find_operator(op.get_text())
                            .expect("operator not found!")
                            .to_string(),
                        args: vec![lhs, rhs],
                    }),
                    id: op_id,
                }
            }
        }
    }

    fn visit_MemberExpr(&mut self, ctx: &MemberExprContext<'_>) -> Self::Return {
        <Self as ParseTreeVisitorCompat>::visit(self, ctx.member().as_deref().unwrap())
    }

    fn visit_LogicalNot(&mut self, ctx: &LogicalNotContext<'_>) -> Self::Return {
        if ctx.ops.len() % 2 == 0 {
            self.visit(ctx.member().as_deref().unwrap());
        }
        let op_id = self.helper.next_id();
        let target = self.visit(ctx.member().as_deref().unwrap());
        IdedExpr {
            expr: Expr::Call(CallExpr {
                target: None,
                func_name: "!_".to_string(),
                args: vec![target],
            }),
            id: op_id,
        }
    }

    fn visit_MemberCall(&mut self, ctx: &MemberCallContext<'_>) -> Self::Return {
        let operand = self.visit(ctx.member().as_deref().unwrap());
        // Handle the error case where no valid identifier is specified.
        // if ctx.id.is_none() {}
        let id = ctx.id.as_deref().unwrap().get_text();

        // return p.receiverCallOrMacro(opID, id, operand, p.visitExprList(ctx.GetArgs())...)
        let op_id = self.helper.next_id();
        let args = ctx
            .args
            .iter()
            .map(|arg| self.visit(arg.deref()))
            .collect::<Vec<IdedExpr>>();
        IdedExpr {
            expr: Expr::Call(CallExpr {
                func_name: id.to_string(),
                target: Some(Box::new(operand)),
                args,
            }),
            id: op_id,
        }
    }

    fn visit_Select(&mut self, ctx: &SelectContext<'_>) -> Self::Return {
        let operand = self.visit(ctx.member().as_deref().unwrap());
        // if ctx.id.is_none() || ctx.op.is_none() {
        // ?
        // }
        let id = ctx.id.as_deref().unwrap().get_text();
        self.helper.next_expr(Expr::Select(SelectExpr {
            op: ctx.op.as_deref().unwrap().text.to_string(),
            operand: Box::new(operand),
            id,
        }))
    }

    fn visit_PrimaryExpr(&mut self, ctx: &PrimaryExprContext<'_>) -> Self::Return {
        <Self as ParseTreeVisitorCompat>::visit(self, ctx.primary().as_deref().unwrap())
    }

    fn visit_Ident(&mut self, ctx: &IdentContext<'_>) -> Self::Return {
        let id = ctx.id.clone().unwrap().text;
        self.helper.next_expr(Expr::Ident(id.to_string()))
    }

    fn visit_Nested(&mut self, ctx: &NestedContext<'_>) -> Self::Return {
        self.visit(ctx.e.as_deref().unwrap())
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

    fn visit_Double(&mut self, ctx: &DoubleContext<'_>) -> Self::Return {
        self.helper.next_expr(Expr::Literal(Val::Double(
            ctx.get_text().parse::<f64>().unwrap(),
        )))
    }

    fn visit_String(&mut self, ctx: &StringContext<'_>) -> Self::Return {
        self.helper
            .next_expr(Expr::Literal(Val::String(ctx.get_text())))
    }

    fn visit_Bytes(&mut self, ctx: &BytesContext<'_>) -> Self::Return {
        let string = ctx.get_text();
        self.helper.next_expr(Expr::Literal(Val::Bytes(
            string.as_bytes()[2..string.len() - 1].to_vec(),
        )))
    }

    fn visit_BoolTrue(&mut self, _ctx: &BoolTrueContext<'_>) -> Self::Return {
        self.helper.next_expr(Expr::Literal(Val::Boolean(true)))
    }

    fn visit_BoolFalse(&mut self, _ctx: &BoolFalseContext<'_>) -> Self::Return {
        self.helper.next_expr(Expr::Literal(Val::Boolean(false)))
    }

    fn visit_Null(&mut self, _: &NullContext<'_>) -> Self::Return {
        self.helper.next_expr(Expr::Literal(Val::Null))
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

struct LogicManager {
    function: String,
    terms: Vec<IdedExpr>,
    ops: Vec<u64>,
}

impl LogicManager {
    pub(crate) fn expr(mut self) -> IdedExpr {
        if self.terms.len() == 1 {
            self.terms.pop().unwrap()
        } else {
            self.balanced_tree(0, self.ops.len() - 1)
        }
    }

    pub(crate) fn add_term(&mut self, op_id: u64, expr: IdedExpr) {
        self.terms.push(expr);
        self.ops.push(op_id);
    }

    fn balanced_tree(&mut self, lo: usize, hi: usize) -> IdedExpr {
        let mid = (lo + hi).div_ceil(2);

        let left = if mid == lo {
            mem::take(&mut self.terms[mid])
        } else {
            self.balanced_tree(lo, mid - 1)
        };

        let right = if mid == hi {
            mem::take(&mut self.terms[mid + 1])
        } else {
            self.balanced_tree(mid + 1, hi)
        };

        IdedExpr {
            id: self.ops[mid],
            expr: Expr::Call(CallExpr {
                target: None,
                func_name: self.function.clone(),
                args: vec![left, right],
            }),
        }
    }
}

fn find_operator(input: &str) -> Option<&str> {
    for (op, operator) in OPERATORS {
        if op == input {
            return Some(operator);
        }
    }
    None
}

const OPERATORS: [(&str, &str); 12] = [
    ("-", "_-_"),
    ("+", "_+_"),
    ("*", "_*_"),
    ("/", "_/_"),
    ("%", "_%_"),
    ("==", "_==_"),
    ("!=", "_!=_"),
    (">=", "_>=_"),
    ("<=", "_<=_"),
    (">", "_>_"),
    ("<", "_<_"),
    ("in", "@in"),
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;
    use crate::reference::Val;
    use std::iter;

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
        let test_cases = [
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
            TestInfo {
                i: "4--4",
                p: r#"_-_(
    4^#1:*expr.Constant_Int64Value#,
    -4^#3:*expr.Constant_Int64Value#
)^#2:*expr.Expr_CallExpr#"#,
            },
            TestInfo {
                i: "4--4.1",
                p: r#"_-_(
    4^#1:*expr.Constant_Int64Value#,
    -4.1^#3:*expr.Constant_DoubleValue#
)^#2:*expr.Expr_CallExpr#"#,
            },
            TestInfo {
                i: r#"b"abc""#,
                p: r#"b"abc"^#1:*expr.Constant_BytesValue#"#,
            },
            TestInfo {
                i: "23.39",
                p: "23.39^#1:*expr.Constant_DoubleValue#",
            },
            TestInfo {
                i: "!a",
                p: "!_(
    a^#2:*expr.Expr_IdentExpr#
)^#1:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "null",
                p: "null^#1:*expr.Constant_NullValue#",
            },
            TestInfo {
                i: "a",
                p: "a^#1:*expr.Expr_IdentExpr#",
            },
            TestInfo {
                i: "a?b:c",
                p: "_?_:_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#,
    c^#4:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a || b",
                p: "_||_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#2:*expr.Expr_IdentExpr#
)^#3:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a || b || c || d || e || f ",
                p: "_||_(
    _||_(
        _||_(
            a^#1:*expr.Expr_IdentExpr#,
            b^#2:*expr.Expr_IdentExpr#
        )^#3:*expr.Expr_CallExpr#,
        c^#4:*expr.Expr_IdentExpr#
    )^#5:*expr.Expr_CallExpr#,
    _||_(
        _||_(
            d^#6:*expr.Expr_IdentExpr#,
            e^#8:*expr.Expr_IdentExpr#
        )^#9:*expr.Expr_CallExpr#,
        f^#10:*expr.Expr_IdentExpr#
    )^#11:*expr.Expr_CallExpr#
)^#7:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a && b",
                p: "_&&_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#2:*expr.Expr_IdentExpr#
)^#3:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a && b && c && d && e && f && g",
                p: "_&&_(
    _&&_(
        _&&_(
            a^#1:*expr.Expr_IdentExpr#,
            b^#2:*expr.Expr_IdentExpr#
        )^#3:*expr.Expr_CallExpr#,
        _&&_(
            c^#4:*expr.Expr_IdentExpr#,
            d^#6:*expr.Expr_IdentExpr#
        )^#7:*expr.Expr_CallExpr#
    )^#5:*expr.Expr_CallExpr#,
    _&&_(
        _&&_(
            e^#8:*expr.Expr_IdentExpr#,
            f^#10:*expr.Expr_IdentExpr#
        )^#11:*expr.Expr_CallExpr#,
        g^#12:*expr.Expr_IdentExpr#
    )^#13:*expr.Expr_CallExpr#
)^#9:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a && b && c && d || e && f && g && h",
                p: "_||_(
    _&&_(
        _&&_(
            a^#1:*expr.Expr_IdentExpr#,
            b^#2:*expr.Expr_IdentExpr#
        )^#3:*expr.Expr_CallExpr#,
        _&&_(
            c^#4:*expr.Expr_IdentExpr#,
            d^#6:*expr.Expr_IdentExpr#
        )^#7:*expr.Expr_CallExpr#
    )^#5:*expr.Expr_CallExpr#,
    _&&_(
        _&&_(
            e^#8:*expr.Expr_IdentExpr#,
            f^#9:*expr.Expr_IdentExpr#
        )^#10:*expr.Expr_CallExpr#,
        _&&_(
            g^#11:*expr.Expr_IdentExpr#,
            h^#13:*expr.Expr_IdentExpr#
        )^#14:*expr.Expr_CallExpr#
    )^#12:*expr.Expr_CallExpr#
)^#15:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a + b",
                p: "_+_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a - b",
                p: "_-_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a * b",
                p: "_*_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a / b",
                p: "_/_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a % b",
                p: "_%_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a in b",
                p: "@in(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a == b",
                p: "_==_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a != b",
                p: "_!=_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a > b",
                p: "_>_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a >= b",
                p: "_>=_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a < b",
                p: "_<_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a <= b",
                p: "_<=_(
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a.b",
                p: "a^#1:*expr.Expr_IdentExpr#.b^#2:*expr.Expr_SelectExpr#",
            },
            TestInfo {
                i: "a.b.c",
                p: "a^#1:*expr.Expr_IdentExpr#.b^#2:*expr.Expr_SelectExpr#.c^#3:*expr.Expr_SelectExpr#",
            },
            TestInfo {
                i: "(a)",
                p: "a^#1:*expr.Expr_IdentExpr#",
            },
            TestInfo {
                i: "((a))",
                p: "a^#1:*expr.Expr_IdentExpr#",
            },
            TestInfo {
                i: "a.b()",
                p: "a^#1:*expr.Expr_IdentExpr#.b()^#2:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a.b(c)",
                p: "a^#1:*expr.Expr_IdentExpr#.b(
    c^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
            },
        ];

        for test_case in test_cases {
            let parser = Parser::new();
            let result = parser.parse(&test_case.i);
            assert_eq!(
                to_go_like_string(&result.unwrap()),
                test_case.p,
                "Expr `{}` failed",
                test_case.i
            );
        }
    }

    fn to_go_like_string(expr: &IdedExpr) -> String {
        let mut writer = DebugWriter::default();
        writer.buffer(expr);
        writer.done()
    }

    struct DebugWriter {
        buffer: String,
        indents: usize,
        line_start: bool,
    }

    impl Default for DebugWriter {
        fn default() -> Self {
            Self {
                buffer: String::default(),
                indents: 0,
                line_start: true,
            }
        }
    }

    impl DebugWriter {
        fn buffer(&mut self, expr: &IdedExpr) -> &Self {
            let e = match &expr.expr {
                Expr::Unspecified => "UNSPECIFIED!",
                Expr::Call(call) => {
                    if let Some(target) = &call.target {
                        self.buffer(target);
                        self.push(".");
                    }
                    self.push(call.func_name.as_str());
                    self.push("(");
                    if call.args.len() > 0 {
                        self.inc_indent();
                        self.newline();
                        for i in 0..call.args.len() {
                            if i > 0 {
                                self.push(",");
                                self.newline();
                            }
                            self.buffer(&call.args[i]);
                        }
                        self.dec_indent();
                        self.newline();
                    }
                    self.push(")");
                    &format!("^#{}:{}#", expr.id, "*expr.Expr_CallExpr")
                }
                Expr::Comprehension => "",
                Expr::Ident(id) => &format!("{}^#{}:{}#", id, expr.id, "*expr.Expr_IdentExpr"),
                Expr::List => "",
                Expr::Literal(val) => match val {
                    Val::String(s) => {
                        &format!("{s}^#{}:{}#", expr.id, "*expr.Constant_StringValue")
                    }
                    Val::Boolean(b) => &format!("{b}^#{}:{}#", expr.id, "*expr.Constant_BoolValue"),
                    Val::Int(i) => &format!("{i}^#{}:{}#", expr.id, "*expr.Constant_Int64Value"),
                    Val::UInt(u) => &format!("{u}u^#{}:{}#", expr.id, "*expr.Constant_Uint64Value"),
                    Val::Double(f) => {
                        &format!("{f}^#{}:{}#", expr.id, "*expr.Constant_DoubleValue")
                    }
                    Val::Bytes(bytes) => &format!(
                        "b\"{}\"^#{}:{}#",
                        String::from_utf8_lossy(bytes),
                        expr.id,
                        "*expr.Constant_BytesValue"
                    ),
                    Val::Null => &format!("null^#{}:{}#", expr.id, "*expr.Constant_NullValue"),
                },
                Expr::Map => "",
                Expr::Select(select) => {
                    self.buffer(select.operand.deref());
                    &format!(
                        "{}{}^#{}:{}#",
                        select.op, select.id, expr.id, "*expr.Expr_SelectExpr"
                    )
                }
                Expr::Struct => "",
            };
            self.push(e);
            self
        }

        fn push(&mut self, literal: &str) {
            self.indent();
            self.buffer.push_str(literal);
        }

        fn indent(&mut self) {
            if self.line_start {
                self.line_start = false;
                self.buffer.push_str(
                    iter::repeat("    ")
                        .take(self.indents)
                        .collect::<String>()
                        .as_str(),
                )
            }
        }

        fn newline(&mut self) {
            self.buffer.push('\n');
            self.line_start = true;
        }

        fn inc_indent(&mut self) {
            self.indents = self.indents + 1;
        }

        fn dec_indent(&mut self) {
            self.indents = self.indents - 1;
        }

        fn done(self) -> String {
            self.buffer
        }
    }
}
