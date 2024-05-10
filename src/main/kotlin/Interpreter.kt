class Interpreter: Expr.Visitor<Any?> {
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
                left + right
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

    private fun truthy(v: Any?): Boolean = v != null && v != false

    private fun assertDouble(token: Token, a: Any?, ifTrue: (Double) -> Any?):Any? =
        if (a is Double) ifTrue(a) else throw InterpreterError(token, "Expected operand to be numbers")

    private fun assertDoubles(token: Token, a: Any?, b: Any?, ifTrue: (Double, Double) -> Any?): Any? =
        if (a is Double && b is Double) ifTrue(a, b) else throw InterpreterError(token, "Expected operands to be numbers")


    fun interpret(expr: Expr): Any? = expr.accept(this)

}

class InterpreterError(val token: Token, message: String) : RuntimeException(message)