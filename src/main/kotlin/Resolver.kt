import ast.Expr
import ast.MatchClause
import ast.Stmt

class Resolver(val resolverFunc: (Expr, Int) -> Unit, private val globals: Set<String>): Expr.Visitor<Unit>, Stmt.Visitor<Unit> {

    private val scopes = ArrayDeque<MutableSet<String>>()

    init {
        scopes.add(mutableSetOf())
    }

    private fun newScope(f: (MutableSet<String>) -> Unit) {
        val s = mutableSetOf<String>()
        scopes.add(s)
        f(s)
        scopes.removeLast()
    }

    override fun visit(binary: Expr.Binary) {
        binary.left.accept(this)
        binary.right.accept(this)
    }

    override fun visit(grouping: Expr.Grouping) = grouping.expr.accept(this)

    override fun visit(literal: Expr.Literal) {}

    override fun visit(literal: Expr.StringLiteral) {
        literal.prefixes.map { it.second }.forEach { it.accept(this) }
    }

    override fun visit(variable: Expr.Variable) = resolveLocal(variable, variable.name)

    override fun visit(unary: Expr.Unary) = unary.right.accept(this)

    override fun visit(ternary: Expr.Ternary) {
        ternary.cond.accept(this)
        ternary.t.accept(this)
        ternary.e.accept(this)
    }

    override fun visit(assign: Expr.Assign) {
        assign.value.accept(this)
        resolveLocal(assign, assign.name)
    }

    override fun visit(call: Expr.Call) {
        call.callee.accept(this)
        call.arguments.forEach { it.accept(this) }
    }

    override fun visit(get: Expr.Get) {
        get.obj.accept(this)
    }

    override fun visit(set: Expr.Set) {
        set.value.accept(this)
        set.obj.accept(this)
    }

    override fun visit(funDef: Expr.FunDef) = newScope { scope ->
        funDef.params.forEach { scope.add(it.lexeme) }
        funDef.body.accept(this)
    }

    override fun visit(t: Expr.This) = resolveLocal(t, t.where)

    override fun visit(expression: Stmt.Expression) = expression.expression.accept(this)

    override fun visit(decl: Stmt.Decl) {
        // resolve initializer before variable name
        decl.init?.accept(this)
        scopes.lastOrNull()?.add(decl.name.lexeme)
    }

    override fun visit(block: Stmt.Block) = newScope {
        block.body.forEach { it.accept(this) }
    }

    override fun visit(ifStmt: Stmt.IfStmt) {
        ifStmt.cond.accept(this)
        ifStmt.t.accept(this)
        ifStmt.e?.accept(this)
    }

    override fun visit(whileStmt: Stmt.WhileStmt) {
        whileStmt.cond.accept(this)
        whileStmt.body?.accept(this)
    }

    private fun visit(matchClause: MatchClause) {
        matchClause.conditions.forEach { it.accept(this) }
        matchClause.body.accept(this)
    }

    override fun visit(matchStmt: Stmt.MatchStmt) {
        matchStmt.obj.accept(this)
        matchStmt.clauses.forEach { visit(it) }
    }

    override fun visit(breakStmt: Stmt.Break) {}

    override fun visit(retStmt: Stmt.Return) {
        retStmt.value?.accept(this)
    }

    override fun visit(funDecl: Stmt.FunDecl) {
        // allow self reference
        scopes.lastOrNull()?.add(funDecl.name.lexeme)
        funDecl.body.accept(this)
    }

    override fun visit(classDecl: Stmt.ClassDecl) {
        scopes.lastOrNull()?.add(classDecl.name.lexeme)

        newScope {
            scopes.lastOrNull()?.add("this")
            classDecl.methods.forEach { visit(it) }
        }
    }

    private fun resolveLocal(expr: Expr, name: Token) {
        val hops = scopes.asReversed().indexOfFirst { it.contains(name.lexeme) }
        // this is maybe a global variable...
        if (hops == -1 && !globals.contains(name.lexeme)) throw ResolverError(name, "Unresolved reference '${name.lexeme}'")
        resolverFunc(expr, hops)
    }

    fun resolve(stmt: Stmt) = stmt.accept(this)

}

class ResolverError(val token: Token, message: String) : RuntimeException(message)