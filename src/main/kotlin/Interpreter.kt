import ast.Expr
import ast.Stmt

class LoxFunction(private val interpreter: Interpreter, private val declaration: Expr.FunDef, private val closure: Environment): LoxCallable {

    private val arity: Int = declaration.params.size

    override fun call(args: List<Any?>): Any? = interpreter.newScope(Environment(closure)) {
        // Bind parameters
        declaration.params.map { it.lexeme }.zip(args).forEach { (param, arg) -> interpreter.env.declare(param, arg) }
        return@newScope try {
            declaration.body.accept(interpreter)
            null
        } catch (e: ReturnException) {
            e.value
        }
    }

    override fun arity(): Int = arity

    override fun toString(): String = "<fun ${declaration.name?.lexeme ?: "anonymous"}#${arity()}>"

    override fun bind(obj: LoxObject): LoxFunction {
        val env = Environment(closure)
        env.declare("this", obj)
        return LoxFunction(interpreter, declaration, env)
    }
}

class Interpreter(internal var env: Environment): Expr.Visitor<Any?>, Stmt.Visitor<Unit> {

    val locals: MutableMap<Expr, Int> = mutableMapOf()


    override fun visit(binary: Expr.Binary): Any? {
        // implement shortcircuiting for `and` and `or`
        when (binary.operator.type) {
            TokenType.AND, TokenType.OR -> {
                val left = binary.left.accept(this)
                val shortCircuit = truthy(left) == (binary.operator.type == TokenType.OR)

                return if (shortCircuit) left else binary.right.accept(this)
            }
            else -> {}
        }

        // other operations do not need short-circuiting
        val left = binary.left.accept(this)
        val right = binary.right.accept(this)
        return when (binary.operator.type) {
            TokenType.COMMA -> right

            TokenType.MINUS -> left as Double - right as Double
            TokenType.PLUS -> if (left is Double && right is Double) {
                left + right
            } else if (left is String) {
                left + right.toJLoxString()
            } else if (right is String) {
                left.toJLoxString() + right
            } else {
                println(left)
                println(right)
                throw InterpreterError(binary.operator,"Expected operands to be numbers or either operand to be a string")
            }

            TokenType.SLASH -> assertDoubles(binary.operator, left, right) { a, b -> a / b }
            TokenType.STAR -> assertDoubles(binary.operator, left, right) { a, b -> a * b }

            TokenType.BANG_EQUAL -> left != right
            TokenType.EQUAL_EQUAL -> left == right
            TokenType.GT -> assertDoubles(binary.operator, left, right) { a, b -> a > b }
            TokenType.GEQ -> assertDoubles(binary.operator, left, right) { a, b -> a >= b }
            TokenType.LT -> assertDoubles(binary.operator, left, right) { a, b -> a < b }
            TokenType.LEQ -> assertDoubles(binary.operator, left, right) { a, b -> a <= b }

            // unreachable
            else -> null
        }
    }

    override fun visit(grouping: Expr.Grouping): Any? = grouping.expr.accept(this)

    override fun visit(literal: Expr.Literal): Any? = literal.value

    override fun visit(literal: Expr.StringLiteral): String =
        literal.prefixes.joinToString("") { (a, b) -> "$a${b.accept(this)}" } + literal.rest

    override fun visit(variable: Expr.Variable): Any?
        = env.get(variable.name.lexeme, locals[variable]!!)

    override fun visit(unary: Expr.Unary): Any? {
        val right = unary.right.accept(this)

        return when (unary.operator.type) {
            TokenType.MINUS -> assertDouble(unary.operator, right) { -it }
            // unreachable
            else -> null
        }
    }

    override fun visit(ternary: Expr.Ternary): Any? {
        val cond = ternary.cond.accept(this)
        return if (truthy(cond)) ternary.t.accept(this) else ternary.e.accept(this)
    }

    override fun visit(assign: Expr.Assign): Any? {
        val value = assign.value.accept(this)
        env.assign(assign.name.lexeme, value, locals[assign]!!)
        return value
    }

    override fun visit(call: Expr.Call): Any? {
        val callee = call.callee.accept(this)
        if (callee !is LoxCallable) {
            throw InterpreterError(call.lparen, "Callee is not a function or a class")
        }

        // evaluate args
        val arity = callee.arity()
        if (arity != null && arity != call.arguments.size) {
            throw InterpreterError(call.lparen, "Expecting $arity arguments, got ${call.arguments.size}")
        }

        return callee.call(call.arguments.map { it.accept(this) })
    }

    override fun visit(get: Expr.Get): Any? {
        val obj = get.obj.accept(this)
        if (obj !is LoxObject) throw InterpreterError(get.name, "Only object have properties")

        return obj.getProp(get.name)
    }

    override fun visit(set: Expr.Set): Any? {
        val obj = set.obj.accept(this)
        if (obj !is LoxObject) throw InterpreterError(set.name, "Only object have properties")

        val rhs = set.value.accept(this)
        obj.setProp(set.name, set.value.accept(this))
        return rhs
    }

    override fun visit(funDef: Expr.FunDef): LoxFunction {
        return LoxFunction(this, funDef, env)
    }

    override fun visit(t: Expr.This): Any?
            = env.get(t.where.lexeme, locals[t]!!)

    private fun assertDouble(token: Token, a: Any?, ifTrue: (Double) -> Any?):Any? =
        if (a is Double) ifTrue(a) else throw InterpreterError(token, "Expected operand to be numbers")

    private fun assertDoubles(token: Token, a: Any?, b: Any?, ifTrue: (Double, Double) -> Any?): Any? =
        if (a is Double && b is Double) ifTrue(a, b) else throw InterpreterError(token, "Expected operands to be numbers")


    override fun visit(expression: Stmt.Expression) {
        expression.expression.accept(this)
    }

    override fun visit(decl: Stmt.Decl) {
        val name = decl.name.lexeme
        env.declare(name, decl.init?.accept(this))
    }

    override fun visit(block: Stmt.Block) {
        newScope(Environment(env)) {
            for (stmt in block.body) {
                stmt.accept(this)
            }
        }
    }

    internal fun newScope(newEnv: Environment, doThing: () -> Any?): Any? {
        val prevEnv = env
        try {
            env = newEnv
            return doThing()
        } finally {
            env = prevEnv
        }
    }

    override fun visit(ifStmt: Stmt.IfStmt) {
        if (truthy(ifStmt.cond.accept(this))) {
            ifStmt.t.accept(this)
        } else {
            ifStmt.e?.accept(this)
        }
    }

    override fun visit(whileStmt: Stmt.WhileStmt) {
        while(truthy(whileStmt.cond.accept(this))) {
            try {
                whileStmt.body?.accept(this)
            } catch (e: BreakException) {
                break
            }
        }
    }

    override fun visit(matchStmt: Stmt.MatchStmt) {
        // first evaluate the obj
        val obj = matchStmt.obj.accept(this)

        // check if any match clauses are equal
        matchStmt.clauses.forEach { clause ->
            if (clause.isDefault) {
                clause.body.accept(this)
                return@visit
            } else if (clause.conditions.any { it.accept(this) == obj }) {
                clause.body.accept(this)
                return@visit
            }
        }
    }

    override fun visit(breakStmt: Stmt.Break) = throw BreakException()

    override fun visit(retStmt: Stmt.Return) = throw ReturnException(retStmt.value?.accept(this))

    override fun visit(funDecl: Stmt.FunDecl) {
        val name = funDecl.name.lexeme
        env.declare(name, funDecl.body.accept(this))
    }

    override fun visit(classDecl: Stmt.ClassDecl) {
        env.declare(classDecl.name.lexeme, null)

        val methods = classDecl.methods.associate { it.name.lexeme to this.visit(it.body) }
        env.assign(classDecl.name.lexeme, LoxClass(classDecl.name.lexeme, methods), skips = 0)
    }

    fun interpret(stmt: Stmt) = stmt.accept(this)
}

class InterpreterError(val token: Token, message: String) : RuntimeException(message)

fun Any?.toJLoxString(): String {
    if (this == null) {
        return "nil"
    }

    if (this is Double) {
        val flr = this.toInt()
        if (flr.toDouble() == this) {
            return flr.toString()
        }
        return this.toString()
    }

    return this.toString()
}