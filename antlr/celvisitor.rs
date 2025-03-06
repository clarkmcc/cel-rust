#![allow(nonstandard_style)]
// Generated from CEL.g4 by ANTLR 4.8
use antlr_rust::tree::{ParseTreeVisitor};
use super::celparser::*;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link CELParser}.
 */
pub trait CELVisitor<'input>: ParseTreeVisitor<'input,CELParserContextType>{
	/**
	 * Visit a parse tree produced by {@link CELParser#start}.
	 * @param ctx the parse tree
	 */
	fn visit_start(&mut self, ctx: &StartContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#expr}.
	 * @param ctx the parse tree
	 */
	fn visit_expr(&mut self, ctx: &ExprContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#conditionalOr}.
	 * @param ctx the parse tree
	 */
	fn visit_conditionalOr(&mut self, ctx: &ConditionalOrContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#conditionalAnd}.
	 * @param ctx the parse tree
	 */
	fn visit_conditionalAnd(&mut self, ctx: &ConditionalAndContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#relation}.
	 * @param ctx the parse tree
	 */
	fn visit_relation(&mut self, ctx: &RelationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#calc}.
	 * @param ctx the parse tree
	 */
	fn visit_calc(&mut self, ctx: &CalcContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code MemberExpr}
	 * labeled alternative in {@link CELParser#unary}.
	 * @param ctx the parse tree
	 */
	fn visit_MemberExpr(&mut self, ctx: &MemberExprContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code LogicalNot}
	 * labeled alternative in {@link CELParser#unary}.
	 * @param ctx the parse tree
	 */
	fn visit_LogicalNot(&mut self, ctx: &LogicalNotContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Negate}
	 * labeled alternative in {@link CELParser#unary}.
	 * @param ctx the parse tree
	 */
	fn visit_Negate(&mut self, ctx: &NegateContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code MemberCall}
	 * labeled alternative in {@link CELParser#member}.
	 * @param ctx the parse tree
	 */
	fn visit_MemberCall(&mut self, ctx: &MemberCallContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Select}
	 * labeled alternative in {@link CELParser#member}.
	 * @param ctx the parse tree
	 */
	fn visit_Select(&mut self, ctx: &SelectContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code PrimaryExpr}
	 * labeled alternative in {@link CELParser#member}.
	 * @param ctx the parse tree
	 */
	fn visit_PrimaryExpr(&mut self, ctx: &PrimaryExprContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Index}
	 * labeled alternative in {@link CELParser#member}.
	 * @param ctx the parse tree
	 */
	fn visit_Index(&mut self, ctx: &IndexContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Ident}
	 * labeled alternative in {@link CELParser#primary}.
	 * @param ctx the parse tree
	 */
	fn visit_Ident(&mut self, ctx: &IdentContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code GlobalCall}
	 * labeled alternative in {@link CELParser#primary}.
	 * @param ctx the parse tree
	 */
	fn visit_GlobalCall(&mut self, ctx: &GlobalCallContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Nested}
	 * labeled alternative in {@link CELParser#primary}.
	 * @param ctx the parse tree
	 */
	fn visit_Nested(&mut self, ctx: &NestedContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code CreateList}
	 * labeled alternative in {@link CELParser#primary}.
	 * @param ctx the parse tree
	 */
	fn visit_CreateList(&mut self, ctx: &CreateListContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code CreateStruct}
	 * labeled alternative in {@link CELParser#primary}.
	 * @param ctx the parse tree
	 */
	fn visit_CreateStruct(&mut self, ctx: &CreateStructContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code CreateMessage}
	 * labeled alternative in {@link CELParser#primary}.
	 * @param ctx the parse tree
	 */
	fn visit_CreateMessage(&mut self, ctx: &CreateMessageContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code ConstantLiteral}
	 * labeled alternative in {@link CELParser#primary}.
	 * @param ctx the parse tree
	 */
	fn visit_ConstantLiteral(&mut self, ctx: &ConstantLiteralContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#exprList}.
	 * @param ctx the parse tree
	 */
	fn visit_exprList(&mut self, ctx: &ExprListContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#listInit}.
	 * @param ctx the parse tree
	 */
	fn visit_listInit(&mut self, ctx: &ListInitContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#fieldInitializerList}.
	 * @param ctx the parse tree
	 */
	fn visit_fieldInitializerList(&mut self, ctx: &FieldInitializerListContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#optField}.
	 * @param ctx the parse tree
	 */
	fn visit_optField(&mut self, ctx: &OptFieldContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#mapInitializerList}.
	 * @param ctx the parse tree
	 */
	fn visit_mapInitializerList(&mut self, ctx: &MapInitializerListContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code SimpleIdentifier}
	 * labeled alternative in {@link CELParser#escapeIdent}.
	 * @param ctx the parse tree
	 */
	fn visit_SimpleIdentifier(&mut self, ctx: &SimpleIdentifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code EscapedIdentifier}
	 * labeled alternative in {@link CELParser#escapeIdent}.
	 * @param ctx the parse tree
	 */
	fn visit_EscapedIdentifier(&mut self, ctx: &EscapedIdentifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link CELParser#optExpr}.
	 * @param ctx the parse tree
	 */
	fn visit_optExpr(&mut self, ctx: &OptExprContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Int}
	 * labeled alternative in {@link CELParser#literal}.
	 * @param ctx the parse tree
	 */
	fn visit_Int(&mut self, ctx: &IntContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Uint}
	 * labeled alternative in {@link CELParser#literal}.
	 * @param ctx the parse tree
	 */
	fn visit_Uint(&mut self, ctx: &UintContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Double}
	 * labeled alternative in {@link CELParser#literal}.
	 * @param ctx the parse tree
	 */
	fn visit_Double(&mut self, ctx: &DoubleContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code String}
	 * labeled alternative in {@link CELParser#literal}.
	 * @param ctx the parse tree
	 */
	fn visit_String(&mut self, ctx: &StringContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Bytes}
	 * labeled alternative in {@link CELParser#literal}.
	 * @param ctx the parse tree
	 */
	fn visit_Bytes(&mut self, ctx: &BytesContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code BoolTrue}
	 * labeled alternative in {@link CELParser#literal}.
	 * @param ctx the parse tree
	 */
	fn visit_BoolTrue(&mut self, ctx: &BoolTrueContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code BoolFalse}
	 * labeled alternative in {@link CELParser#literal}.
	 * @param ctx the parse tree
	 */
	fn visit_BoolFalse(&mut self, ctx: &BoolFalseContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by the {@code Null}
	 * labeled alternative in {@link CELParser#literal}.
	 * @param ctx the parse tree
	 */
	fn visit_Null(&mut self, ctx: &NullContext<'input>) { self.visit_children(ctx) }


}