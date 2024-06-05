package ast

import Token

sealed class Expr {
    class Binary(val operator: Token, val left: Expr, val right: Expr): Expr()
    class Grouping(val expr: Expr): Expr()
    class Literal(val value: Any?): Expr()
    class StringLiteral(val prefixes: List<Pair<String, Expr>>, val rest: String): Expr()
    class Variable(val name: Token): Expr()
    class Unary(val operator: Token, val right: Expr): Expr()
    class Ternary(val cond: Expr, val t: Expr, val e: Expr): Expr()
    class Assign(val name: Token, val value: Expr): Expr()
    class Call(val callee: Expr, val lparen: Token, val arguments: List<Expr>): Expr()
    class Get(val obj: Expr, val name: Token): Expr()
    class Set(val obj: Expr, val name: Token, val value: Expr): Expr()
    class This(val where: Token): Expr()
    class FunDef(val name: Token?, val params: List<Token>, val body: Stmt): Expr()

    fun <R> accept(visitor: Visitor<R>): R = when (this) {
        is Binary -> visitor.visit(this)
        is Grouping -> visitor.visit(this)
        is Literal -> visitor.visit(this)
        is StringLiteral -> visitor.visit(this)
        is Variable -> visitor.visit(this)
        is Unary -> visitor.visit(this)
        is Ternary -> visitor.visit(this)
        is Assign -> visitor.visit(this)
        is Call -> visitor.visit(this)
        is Get -> visitor.visit(this)
        is FunDef -> visitor.visit(this)
        is Set -> visitor.visit(this)
        is This -> visitor.visit(this)
    }

    interface Visitor<R> {
        fun visit(binary: Binary): R
        fun visit(grouping: Grouping): R
        fun visit(literal: Literal): R
        fun visit(literal: StringLiteral): R
        fun visit(variable: Variable): R
        fun visit(unary: Unary): R
        fun visit(ternary: Ternary): R
        fun visit(assign: Assign): R
        fun visit(call: Call): R
        fun visit(get: Get): R
        fun visit(set: Set): R
        fun visit(funDef: FunDef): R
        fun visit(t: This): R
    }
}