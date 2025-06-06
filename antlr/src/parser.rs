use crate::ast::{
    operators, CallExpr, EntryExpr, Expr, IdedEntryExpr, IdedExpr, ListExpr, MapEntryExpr, MapExpr,
    SelectExpr, StructExpr, StructFieldExpr,
};
use crate::gen::{
    BoolFalseContext, BoolTrueContext, BytesContext, CalcContext, CalcContextAttrs,
    ConditionalAndContext, ConditionalOrContext, ConstantLiteralContext,
    ConstantLiteralContextAttrs, CreateListContext, CreateMessageContext, CreateStructContext,
    DoubleContext, ExprContext, FieldInitializerListContext, GlobalCallContext, IdentContext,
    IndexContext, IndexContextAttrs, IntContext, ListInitContextAll, LogicalNotContext,
    LogicalNotContextAttrs, MapInitializerListContextAll, MemberCallContext,
    MemberCallContextAttrs, MemberExprContext, MemberExprContextAttrs, NegateContext,
    NegateContextAttrs, NestedContext, NullContext, OptFieldContextAttrs, PrimaryExprContext,
    PrimaryExprContextAttrs, RelationContext, RelationContextAttrs, SelectContext,
    SelectContextAttrs, StartContext, StartContextAttrs, StringContext, UintContext,
};
use crate::reference::Val;
use crate::{ast, gen, macros, parse};
use antlr4rust::common_token_stream::CommonTokenStream;
use antlr4rust::parser::ParserNodeType;
use antlr4rust::token::Token;
use antlr4rust::tree::{ParseTree, ParseTreeVisitorCompat, VisitChildren};
use antlr4rust::InputStream;
use std::mem;
use std::ops::Deref;

type MacroExpander =
    fn(helper: &mut ParserHelper, target: Option<IdedExpr>, args: Vec<IdedExpr>) -> IdedExpr;

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

    fn new_logic_manager(&self, func: &str, term: IdedExpr) -> LogicManager {
        LogicManager {
            function: func.to_string(),
            terms: vec![term],
            ops: vec![],
        }
    }

    fn global_call_or_macro(
        &mut self,
        id: u64,
        func_name: String,
        args: Vec<IdedExpr>,
    ) -> IdedExpr {
        match self.macro_expander(&func_name, None, &args) {
            None => IdedExpr {
                id,
                expr: Expr::Call(CallExpr {
                    target: None,
                    func_name,
                    args,
                }),
            },
            Some(expander) => expander(&mut self.helper, None, args),
        }
    }

    fn receiver_call_or_macro(
        &mut self,
        id: u64,
        func_name: String,
        target: IdedExpr,
        args: Vec<IdedExpr>,
    ) -> IdedExpr {
        match self.macro_expander(&func_name, Some(&target), &args) {
            None => IdedExpr {
                id,
                expr: Expr::Call(CallExpr {
                    target: Some(Box::new(target)),
                    func_name,
                    args,
                }),
            },
            Some(expander) => expander(&mut self.helper, Some(target), args),
        }
    }

    fn macro_expander(
        &mut self,
        func_name: &str,
        target: Option<&IdedExpr>,
        args: &[IdedExpr],
    ) -> Option<MacroExpander> {
        match func_name {
            operators::HAS if args.len() == 1 && target.is_none() => {
                Some(macros::has_macro_expander)
            }
            operators::EXISTS if args.len() == 2 && target.is_some() => {
                Some(macros::exists_macro_expander)
            }
            operators::ALL if args.len() == 2 && target.is_some() => {
                Some(macros::all_macro_expander)
            }
            operators::EXISTS_ONE | "existsOne" if args.len() == 2 && target.is_some() => {
                Some(macros::exists_one_macro_expander)
            }
            operators::MAP if (args.len() == 2 || args.len() == 3) && target.is_some() => {
                Some(macros::map_macro_expander)
            }
            operators::FILTER if args.len() == 2 && target.is_some() => {
                Some(macros::filter_macro_expander)
            }
            _ => None,
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

    fn field_initializer_list(
        &mut self,
        ctx: &FieldInitializerListContext<'_>,
    ) -> Vec<IdedEntryExpr> {
        let mut fields = Vec::with_capacity(ctx.fields.len());
        for (i, field) in ctx.fields.iter().enumerate() {
            if i >= ctx.cols.len() || i >= ctx.values.len() {
                return vec![];
            }
            let id = self.helper.next_id();
            let field = field.escapeIdent().unwrap().get_text().to_string();
            let value = self.visit(ctx.values[i].as_ref());
            fields.push(IdedEntryExpr {
                id,
                expr: EntryExpr::StructField(StructFieldExpr {
                    field,
                    value,
                    optional: false,
                }),
            });
        }
        fields
    }

    fn map_initializer_list(&mut self, ctx: &MapInitializerListContextAll) -> Vec<IdedEntryExpr> {
        if ctx.keys.is_empty() {
            return vec![];
        }
        let mut entries = Vec::with_capacity(ctx.cols.len());
        let keys = &ctx.keys;
        let vals = &ctx.values;
        for (i, _col) in ctx.cols.iter().enumerate() {
            if i >= keys.len() || i >= vals.len() {
                return vec![];
            }
            if keys[i].opt.is_some() {
                todo!("No support for `?` optional")
            }
            let id = self.helper.next_id();
            let key = self.visit(keys[i].as_ref());
            let value = self.visit(vals[i].as_ref());
            entries.push(IdedEntryExpr {
                id,
                expr: EntryExpr::MapEntry(MapEntryExpr {
                    key,
                    value,
                    optional: false,
                }),
            })
        }
        entries
    }

    fn list_initializer_list(&mut self, ctx: &ListInitContextAll) -> Vec<IdedExpr> {
        ctx.elems
            .iter()
            .map(|e| self.visit(e.e.as_deref().unwrap()))
            .collect()
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
            self.global_call_or_macro(
                op_id,
                operators::CONDITIONAL.to_string(),
                vec![result, if_true, if_false],
            )
        }
    }

    fn visit_conditionalOr(&mut self, ctx: &ConditionalOrContext<'_>) -> Self::Return {
        if ctx.ops.is_empty() {
            <Self as ParseTreeVisitorCompat>::visit(self, ctx.e.as_deref().unwrap())
        } else {
            let result = self.visit(ctx.e.as_deref().unwrap());
            let mut l = self.new_logic_manager(operators::LOGICAL_OR, result);
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
            let mut l = self.new_logic_manager(operators::LOGICAL_AND, result);
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
                    self.global_call_or_macro(
                        op_id,
                        operators::find_operator(op.get_text())
                            .expect("operator not found!")
                            .to_string(),
                        vec![lhs, rhs],
                    )
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
                self.global_call_or_macro(
                    op_id,
                    operators::find_operator(op.get_text())
                        .expect("operator not found!")
                        .to_string(),
                    vec![lhs, rhs],
                )
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
        self.global_call_or_macro(op_id, operators::LOGICAL_NOT.to_string(), vec![target])
    }

    fn visit_Negate(&mut self, ctx: &NegateContext<'_>) -> Self::Return {
        if ctx.ops.len() % 2 == 0 {
            self.visit(ctx.member().as_deref().unwrap());
        }
        let op_id = self.helper.next_id();
        let target = self.visit(ctx.member().as_deref().unwrap());
        self.global_call_or_macro(op_id, operators::NEGATE.to_string(), vec![target])
    }

    fn visit_MemberCall(&mut self, ctx: &MemberCallContext<'_>) -> Self::Return {
        let operand = self.visit(ctx.member().as_deref().unwrap());
        // Handle the error case where no valid identifier is specified.
        // if ctx.id.is_none() {}
        let id = ctx.id.as_deref().unwrap().get_text();

        let op_id = self.helper.next_id();
        let args = ctx
            .args
            .iter()
            .flat_map(|arg| &arg.e)
            .map(|arg| self.visit(arg.deref()))
            .collect::<Vec<IdedExpr>>();
        self.receiver_call_or_macro(op_id, id.to_string(), operand, args)
    }

    fn visit_Select(&mut self, ctx: &SelectContext<'_>) -> Self::Return {
        let operand = self.visit(ctx.member().as_deref().unwrap());
        // if ctx.id.is_none() || ctx.op.is_none() {
        // ?
        // }
        let field = ctx.id.as_deref().unwrap().get_text();
        self.helper.next_expr(Expr::Select(SelectExpr {
            operand: Box::new(operand),
            field,
            test: false,
        }))
    }

    fn visit_PrimaryExpr(&mut self, ctx: &PrimaryExprContext<'_>) -> Self::Return {
        <Self as ParseTreeVisitorCompat>::visit(self, ctx.primary().as_deref().unwrap())
    }

    fn visit_Index(&mut self, ctx: &IndexContext<'_>) -> Self::Return {
        let target = self.visit(ctx.member().as_deref().unwrap());
        let op_id = self.helper.next_id();
        match &ctx.op {
            None => IdedExpr {
                id: op_id,
                expr: Expr::default(),
            },
            Some(_) => {
                let index = self.visit(ctx.index.as_deref().unwrap());
                self.global_call_or_macro(op_id, operators::INDEX.to_string(), vec![target, index])
            }
        }
    }

    fn visit_Ident(&mut self, ctx: &IdentContext<'_>) -> Self::Return {
        let id = ctx.id.clone().unwrap().text;
        self.helper.next_expr(Expr::Ident(id.to_string()))
    }

    fn visit_GlobalCall(&mut self, ctx: &GlobalCallContext<'_>) -> Self::Return {
        match &ctx.id {
            None => self.helper.next_expr(Expr::default()),
            Some(id) => {
                let mut id = id.get_text().to_string();
                if ctx.leadingDot.is_some() {
                    id = format!(".{}", id);
                }
                let op_id = self.helper.next_id();
                let args = ctx
                    .args
                    .iter()
                    .flat_map(|arg| &arg.e)
                    .map(|arg| self.visit(arg.deref()))
                    .collect::<Vec<IdedExpr>>();
                self.global_call_or_macro(op_id, id, args)
            }
        }
    }

    fn visit_Nested(&mut self, ctx: &NestedContext<'_>) -> Self::Return {
        self.visit(ctx.e.as_deref().unwrap())
    }

    fn visit_CreateList(&mut self, ctx: &CreateListContext<'_>) -> Self::Return {
        let list_id = self.helper.next_id();
        let elements = match &ctx.elems {
            None => Vec::default(),
            Some(elements) => self.list_initializer_list(elements.deref()),
        };
        IdedExpr {
            id: list_id,
            expr: Expr::List(ListExpr { elements }),
        }
    }

    fn visit_CreateStruct(&mut self, ctx: &CreateStructContext<'_>) -> Self::Return {
        let struct_id = self.helper.next_id();
        let entries = match &ctx.entries {
            Some(entries) => self.map_initializer_list(entries.deref()),
            None => Vec::default(),
        };
        IdedExpr {
            id: struct_id,
            expr: Expr::Map(MapExpr { entries }),
        }
    }

    fn visit_CreateMessage(&mut self, ctx: &CreateMessageContext<'_>) -> Self::Return {
        let mut message_name = String::new();
        for id in &ctx.ids {
            if !message_name.is_empty() {
                message_name.push('.');
            }
            message_name.push_str(id.get_text());
        }
        if ctx.leadingDot.is_some() {
            message_name = format!(".{}", message_name);
        }
        let op_id = self.helper.next_id();
        let entries = match &ctx.entries {
            None => vec![],
            Some(entries) => self.field_initializer_list(entries),
        };
        IdedExpr {
            id: op_id,
            expr: Expr::Struct(StructExpr {
                type_name: message_name,
                entries,
            }),
        }
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
            .next_expr(Expr::Literal(Val::String(parse::parse_string(&ctx.get_text()).expect("invalid string"))))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ComprehensionExpr, Expr};
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
                i: "a[b]",
                p: "_[_](
    a^#1:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#
)^#2:*expr.Expr_CallExpr#",
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
                i: "a()",
                p: "a()^#1:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a(b)",
                p: "a(
    b^#2:*expr.Expr_IdentExpr#
)^#1:*expr.Expr_CallExpr#",
            },
            TestInfo {
                i: "a(b, c)",
                p: "a(
    b^#2:*expr.Expr_IdentExpr#,
    c^#3:*expr.Expr_IdentExpr#
)^#1:*expr.Expr_CallExpr#",
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
            TestInfo {
                i: "foo{ }",
                p: "foo{}^#1:*expr.Expr_StructExpr#",
            },
            TestInfo {
                i: "foo{ a:b }",
                p: "foo{
    a:b^#3:*expr.Expr_IdentExpr#^#2:*expr.Expr_CreateStruct_Entry#
}^#1:*expr.Expr_StructExpr#",
            },
            TestInfo {
                i: "foo{ a:b, c:d }",
                p: "foo{
    a:b^#3:*expr.Expr_IdentExpr#^#2:*expr.Expr_CreateStruct_Entry#,
    c:d^#5:*expr.Expr_IdentExpr#^#4:*expr.Expr_CreateStruct_Entry#
}^#1:*expr.Expr_StructExpr#",
            },
            TestInfo {
                i: "{}",
                p: "{}^#1:*expr.Expr_StructExpr#",
            },
            TestInfo {
                i: "{a: b, c: d}",
                p: "{
    a^#3:*expr.Expr_IdentExpr#:b^#4:*expr.Expr_IdentExpr#^#2:*expr.Expr_CreateStruct_Entry#,
    c^#6:*expr.Expr_IdentExpr#:d^#7:*expr.Expr_IdentExpr#^#5:*expr.Expr_CreateStruct_Entry#
}^#1:*expr.Expr_StructExpr#",
            },
            TestInfo {
                i: "[]",
                p: "[]^#1:*expr.Expr_ListExpr#",
            },
            TestInfo {
                i: "[a]",
                p: "[
    a^#2:*expr.Expr_IdentExpr#
]^#1:*expr.Expr_ListExpr#",
            },
            TestInfo {
                i: "[a, b, c]",
                p: "[
    a^#2:*expr.Expr_IdentExpr#,
    b^#3:*expr.Expr_IdentExpr#,
    c^#4:*expr.Expr_IdentExpr#
]^#1:*expr.Expr_ListExpr#",
            },
            TestInfo {
                i: "has(m.f)",
                p: "m^#2:*expr.Expr_IdentExpr#.f~test-only~^#4:*expr.Expr_SelectExpr#",
            },
            TestInfo {
                i: "m.exists(v, f)",
                p: "__comprehension__(
// Variable
v,
// Target
m^#1:*expr.Expr_IdentExpr#,
// Accumulator
@result,
// Init
false^#5:*expr.Constant_BoolValue#,
// LoopCondition
@not_strictly_false(
    !_(
        @result^#6:*expr.Expr_IdentExpr#
    )^#7:*expr.Expr_CallExpr#
)^#8:*expr.Expr_CallExpr#,
// LoopStep
_||_(
    @result^#9:*expr.Expr_IdentExpr#,
    f^#4:*expr.Expr_IdentExpr#
)^#10:*expr.Expr_CallExpr#,
// Result
@result^#11:*expr.Expr_IdentExpr#)^#12:*expr.Expr_ComprehensionExpr#",
            },
            TestInfo {
                i: "m.all(v, f)",
                p: "__comprehension__(
// Variable
v,
// Target
m^#1:*expr.Expr_IdentExpr#,
// Accumulator
@result,
// Init
true^#5:*expr.Constant_BoolValue#,
// LoopCondition
@not_strictly_false(
    @result^#6:*expr.Expr_IdentExpr#
)^#7:*expr.Expr_CallExpr#,
// LoopStep
_&&_(
    @result^#8:*expr.Expr_IdentExpr#,
    f^#4:*expr.Expr_IdentExpr#
)^#9:*expr.Expr_CallExpr#,
// Result
@result^#10:*expr.Expr_IdentExpr#)^#11:*expr.Expr_ComprehensionExpr#",
            },
            TestInfo {
                i: "m.existsOne(v, f)",
                p: "__comprehension__(
// Variable
v,
// Target
m^#1:*expr.Expr_IdentExpr#,
// Accumulator
@result,
// Init
0^#5:*expr.Constant_Int64Value#,
// LoopCondition
true^#6:*expr.Constant_BoolValue#,
// LoopStep
_?_:_(
    f^#4:*expr.Expr_IdentExpr#,
    _+_(
        @result^#7:*expr.Expr_IdentExpr#,
        1^#8:*expr.Constant_Int64Value#
    )^#9:*expr.Expr_CallExpr#,
    @result^#10:*expr.Expr_IdentExpr#
)^#11:*expr.Expr_CallExpr#,
// Result
_==_(
    @result^#12:*expr.Expr_IdentExpr#,
    1^#13:*expr.Constant_Int64Value#
)^#14:*expr.Expr_CallExpr#)^#15:*expr.Expr_ComprehensionExpr#",  
            },
            TestInfo {
                i: "m.map(v, f)",
                p: "__comprehension__(
// Variable
v,
// Target
m^#1:*expr.Expr_IdentExpr#,
// Accumulator
@result,
// Init
[]^#5:*expr.Expr_ListExpr#,
// LoopCondition
true^#6:*expr.Constant_BoolValue#,
// LoopStep
_+_(
    @result^#7:*expr.Expr_IdentExpr#,
    [
        f^#4:*expr.Expr_IdentExpr#
    ]^#8:*expr.Expr_ListExpr#
)^#9:*expr.Expr_CallExpr#,
// Result
@result^#10:*expr.Expr_IdentExpr#)^#11:*expr.Expr_ComprehensionExpr#",
            },
            TestInfo {
                i: "m.map(v, p, f)",
                p: "__comprehension__(
// Variable
v,
// Target
m^#1:*expr.Expr_IdentExpr#,
// Accumulator
@result,
// Init
[]^#6:*expr.Expr_ListExpr#,
// LoopCondition
true^#7:*expr.Constant_BoolValue#,
// LoopStep
_?_:_(
    p^#4:*expr.Expr_IdentExpr#,
    _+_(
        @result^#8:*expr.Expr_IdentExpr#,
        [
            f^#5:*expr.Expr_IdentExpr#
        ]^#9:*expr.Expr_ListExpr#
    )^#10:*expr.Expr_CallExpr#,
    @result^#11:*expr.Expr_IdentExpr#
)^#12:*expr.Expr_CallExpr#,
// Result
@result^#13:*expr.Expr_IdentExpr#)^#14:*expr.Expr_ComprehensionExpr#",
            },
            TestInfo {
                i: "m.filter(v, p)",
                p: "__comprehension__(
// Variable
v,
// Target
m^#1:*expr.Expr_IdentExpr#,
// Accumulator
@result,
// Init
[]^#5:*expr.Expr_ListExpr#,
// LoopCondition
true^#6:*expr.Constant_BoolValue#,
// LoopStep
_?_:_(
    p^#4:*expr.Expr_IdentExpr#,
    _+_(
        @result^#7:*expr.Expr_IdentExpr#,
        [
            v^#3:*expr.Expr_IdentExpr#
        ]^#8:*expr.Expr_ListExpr#
    )^#9:*expr.Expr_CallExpr#,
    @result^#10:*expr.Expr_IdentExpr#
)^#11:*expr.Expr_CallExpr#,
// Result
@result^#12:*expr.Expr_IdentExpr#)^#13:*expr.Expr_ComprehensionExpr#",
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
                Expr::Comprehension(comprehension) => {
                    self.push("__comprehension__(\n");
                    self.push_comprehension(comprehension);
                    &format!(")^#{}:{}#", expr.id, "*expr.Expr_ComprehensionExpr")
                }
                Expr::Ident(id) => &format!("{}^#{}:{}#", id, expr.id, "*expr.Expr_IdentExpr"),
                Expr::List(list) => {
                    self.push("[");
                    if list.elements.len() > 0 {
                        self.inc_indent();
                        self.newline();
                        for (i, element) in list.elements.iter().enumerate() {
                            if i > 0 {
                                self.push(",");
                                self.newline();
                            }
                            self.buffer(element);
                        }
                        self.dec_indent();
                        self.newline();
                    }
                    self.push("]");
                    &format!("^#{}:{}#", expr.id, "*expr.Expr_ListExpr")
                }
                Expr::Literal(val) => match val {
                    Val::String(s) => {
                        &format!("\"{s}\"^#{}:{}#", expr.id, "*expr.Constant_StringValue")
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
                Expr::Map(map) => {
                    self.push("{");
                    self.inc_indent();
                    if !map.entries.is_empty() {
                        self.newline();
                    }
                    for (i, entry) in map.entries.iter().enumerate() {
                        match &entry.expr {
                            EntryExpr::StructField(_) => panic!("WAT?!"),
                            EntryExpr::MapEntry(e) => {
                                self.buffer(&e.key);
                                self.push(":");
                                self.buffer(&e.value);
                                self.push(&format!(
                                    "^#{}:{}#",
                                    entry.id, "*expr.Expr_CreateStruct_Entry"
                                ));
                            }
                        }
                        if i < map.entries.len() - 1 {
                            self.push(",");
                        }
                        self.newline();
                    }
                    self.dec_indent();
                    self.push("}");
                    &format!("^#{}:{}#", expr.id, "*expr.Expr_StructExpr")
                }
                Expr::Select(select) => {
                    self.buffer(select.operand.deref());
                    let suffix = if select.test { "~test-only~" } else { "" };
                    &format!(
                        ".{}{}^#{}:{}#",
                        select.field, suffix, expr.id, "*expr.Expr_SelectExpr"
                    )
                }
                Expr::Struct(s) => {
                    self.push(&s.type_name);
                    self.push("{");
                    self.inc_indent();
                    if !s.entries.is_empty() {
                        self.newline();
                    }
                    for (i, entry) in s.entries.iter().enumerate() {
                        match &entry.expr {
                            EntryExpr::StructField(field) => {
                                self.push(&field.field);
                                self.push(":");
                                self.buffer(&field.value);
                                self.push(&format!(
                                    "^#{}:{}#",
                                    entry.id, "*expr.Expr_CreateStruct_Entry"
                                ));
                            }
                            EntryExpr::MapEntry(_) => panic!("WAT?!"),
                        }
                        if i < s.entries.len() - 1 {
                            self.push(",");
                        }
                        self.newline();
                    }
                    self.dec_indent();
                    self.push("}");
                    &format!("^#{}:{}#", expr.id, "*expr.Expr_StructExpr")
                }
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

        fn push_comprehension(&mut self, comprehension: &ComprehensionExpr) {
            self.push("// Variable\n");
            self.push(comprehension.iter_var.as_str());
            self.push(",\n");
            self.push("// Target\n");
            self.buffer(comprehension.iter_range.as_ref());
            self.push(",\n");
            self.push("// Accumulator\n");
            self.push(comprehension.accu_var.as_str());
            self.push(",\n");
            self.push("// Init\n");
            self.buffer(comprehension.accu_init.as_ref());
            self.push(",\n");
            self.push("// LoopCondition\n");
            self.buffer(comprehension.loop_cond.as_ref());
            self.push(",\n");
            self.push("// LoopStep\n");
            self.buffer(comprehension.loop_step.as_ref());
            self.push(",\n");
            self.push("// Result\n");
            self.buffer(comprehension.result.as_ref());
        }
    }
}
