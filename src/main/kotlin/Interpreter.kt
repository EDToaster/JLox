class Interpreter(private var env: Environment): Expr.Visitor<Any?>, Stmt.Visitor<Unit> {

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
            } else {
                throw InterpreterError(binary.operator,"Expected operands to be numbers or first operand to be a string")
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

    override fun visit(variable: Expr.Variable): Any? = env[variable.name.lexeme]

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
        env.assign(assign.name.lexeme, value)
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

    override fun visit(funDef: Expr.FunDef): Any {
        val closure = env
        return object: LoxCallable {
            override fun call(args: List<Any?>): Any? = newScope(Environment(closure)) {
                // Bind parameters
                funDef.params.map { it.lexeme }.zip(args).forEach { (param, arg) -> env.declare(param, arg) }
                return@newScope try {
                    funDef.body.accept(this@Interpreter)
                    null
                } catch (e: ReturnException) {
                    e.value
                }
            }

            override fun arity(): Int = funDef.params.size

            override fun toString(): String = "<fun anonymous#${arity()}>"
        }
    }

    private fun truthy(v: Any?): Boolean = v != null && v != false

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

    private fun newScope(newEnv: Environment, doThing: () -> Any?): Any? {
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

    override fun visit(breakStmt: Stmt.Break) = throw BreakException()

    override fun visit(retStmt: Stmt.Return) = throw ReturnException(retStmt.value?.accept(this))

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