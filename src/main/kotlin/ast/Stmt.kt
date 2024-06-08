package ast

import Token

data class MatchClause(val isDefault: Boolean, val conditions: List<Expr>, val body: Stmt)

sealed class Stmt {
    data class Expression(val expression: Expr): Stmt()
    data class Decl(val name: Token, val init: Expr?): Stmt()
    data class FunDecl(val name: Token, val body: Expr.FunDef): Stmt()
    data class ClassDecl(val name: Token, val methods: List<FunDecl>): Stmt()
    data class Block(val body: List<Stmt>): Stmt()
    data class IfStmt(val cond: Expr, val t: Stmt, val e: Stmt?): Stmt()
    data class WhileStmt(val cond: Expr, val body: Stmt?): Stmt()
    data class MatchStmt(val obj: Expr, val clauses: List<MatchClause>): Stmt()
    data object Break : Stmt()
    data class Return(val value: Expr?): Stmt()

    fun <R> accept(visitor: Visitor<R>): R = when (this) {
        is Expression -> visitor.visit(this)
        is Decl -> visitor.visit(this)
        is FunDecl -> visitor.visit(this)
        is ClassDecl -> visitor.visit(this)
        is Block -> visitor.visit(this)
        is IfStmt -> visitor.visit(this)
        is WhileStmt -> visitor.visit(this)
        is MatchStmt -> visitor.visit(this)
        is Break -> visitor.visit(this)
        is Return -> visitor.visit(this)
    }

    interface Visitor<R> {
        fun visit(expression: Expression): R
        fun visit(decl: Decl): R
        fun visit(funDecl: FunDecl): R
        fun visit(classDecl: ClassDecl): R
        fun visit(block: Block): R
        fun visit(ifStmt: IfStmt): R
        fun visit(whileStmt: WhileStmt): R
        fun visit(matchStmt: MatchStmt): R
        fun visit(breakStmt: Break): R
        fun visit(retStmt: Return): R
    }
}