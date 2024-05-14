package ast

import Token

sealed class Stmt {
    class Expression(val expression: Expr): Stmt()
    class Decl(val name: Token, val init: Expr?): Stmt()
    class FunDecl(val name: Token, val body: Expr.FunDef): Stmt()
    class ClassDecl(val name: Token, val methods: List<FunDecl>): Stmt()
    class Block(val body: List<Stmt>): Stmt()
    class IfStmt(val cond: Expr, val t: Stmt, val e: Stmt?): Stmt()
    class WhileStmt(val cond: Expr, val body: Stmt?): Stmt()
    data object Break : Stmt()
    class Return(val value: Expr?): Stmt()

    fun <R> accept(visitor: Visitor<R>): R = when (this) {
        is Expression -> visitor.visit(this)
        is Decl -> visitor.visit(this)
        is FunDecl -> visitor.visit(this)
        is ClassDecl -> visitor.visit(this)
        is Block -> visitor.visit(this)
        is IfStmt -> visitor.visit(this)
        is WhileStmt -> visitor.visit(this)
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
        fun visit(breakStmt: Break): R
        fun visit(retStmt: Return): R
    }
}