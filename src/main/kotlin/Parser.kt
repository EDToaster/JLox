sealed class Expr {
    class Binary(val operator: Token, val left: Expr, val right: Expr): Expr()
    class Grouping(val expr: Expr): Expr()
    class Literal(val value: Any?): Expr()
    class Unary(val operator: Token, val right: Expr): Expr()
    class Ternary(val cond: Expr, val t: Expr, val e: Expr): Expr()

    fun <R> accept(visitor: Visitor<R>): R {
        return when (this) {
            is Binary -> visitor.visit(this)
            is Grouping -> visitor.visit(this)
            is Literal -> visitor.visit(this)
            is Unary -> visitor.visit(this)
            is Ternary -> visitor.visit(this)
        }
    }

    interface Visitor<R> {
        fun visit(binary: Binary): R
        fun visit(grouping: Grouping): R
        fun visit(literal: Literal): R
        fun visit(unary: Unary): R
        fun visit(ternary: Ternary): R
    }
}

class ASTPrinter: Expr.Visitor<String> {
    override fun visit(binary: Expr.Binary): String {
        return "(${binary.operator.lexeme} ${binary.left.accept(this)} ${binary.right.accept(this)})"
    }

    override fun visit(grouping: Expr.Grouping): String {
        return "(group ${grouping.expr.accept(this)})"
    }

    override fun visit(literal: Expr.Literal): String {
        return "${literal.value ?: "nil"}"
    }

    override fun visit(unary: Expr.Unary): String {
        return "(${unary.operator.lexeme} ${unary.right.accept(this)})"
    }

    override fun visit(ternary: Expr.Ternary): String {
        return "(${ternary.cond.accept(this)} ? ${ternary.t.accept(this)} : ${ternary.e.accept(this)})"
    }
}

class Parser(private val tokens: List<Token>, val reportError: (Int, String, String) -> Unit) {
    private var current = 0

    private fun peek(): Token = tokens[current]

    private fun previous(): Token = tokens[current - 1]

    private fun isAtEnd(): Boolean = peek().type == TokenType.EOF

    private fun check(type: TokenType): Boolean = !isAtEnd() && peek().type == type

    private fun advance(): Token {
        if (!isAtEnd()) current++
        return previous()
    }

    private fun match(vararg possibleTypes: TokenType): Boolean {
        val m = possibleTypes.any(::check)
        if (m) advance()
        return m
    }

    private fun binop(production: () -> Expr, vararg possibleTypes: TokenType): Expr {
        var left = production()
        while (match(*possibleTypes)) {
            val op = previous()
            val right = production()
            left = Expr.Binary(op, left, right)
        }
        return left
    }

    /*
     * expression     → sequence ;
     * sequence       → andor ( "," andor )* ;
     * andor          → ternary ( ( "or" | "and" ) ternary)*
     * ternary        → equality ( "?" expression ":" ternary )? ;
     * equality       → comparison ( ( "!=" | "==" ) comparison )* ;
     * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
     * term           → factor ( ( "-" | "+" ) factor )* ;
     * factor         → unary ( ( "/" | "*" ) unary )* ;
     * unary          → ( "!" | "-" ) unary
     *                | primary ;
     * primary        → NUMBER | STRING | "true" | "false" | "nil"
     *                | "(" expression ")" ;
     *                # ERROR PRODUCTION RULES
     *                | ( "!=" | "==" ) equality
     *                | ( ">" | ">=" | "<" | "<=" ) comparison
     *                | "+" term
     *                | ( "/" | "*" ) factor
     */

    private fun expression(): Expr = sequence()
    private fun sequence(): Expr = binop(::andor, TokenType.COMMA)
    private fun andor(): Expr = binop(::ternary, TokenType.AND, TokenType.OR)

    private fun ternary(): Expr {
        var left = equality()
        if (match(TokenType.QUESTION)) {
            val t = expression()
            consume(TokenType.COLON, "Expected ':' in ternary expression")
            val e = ternary()
            left = Expr.Ternary(left, t, e)
        }
        return left
    }

    private fun equality(): Expr = binop(::comparison, TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)
    private fun comparison(): Expr = binop(::term, TokenType.GT, TokenType.GEQ, TokenType.LT, TokenType.LEQ)
    private fun term(): Expr = binop(::factor, TokenType.MINUS, TokenType.PLUS)
    private fun factor(): Expr = binop(::unary, TokenType.SLASH, TokenType.STAR)

    private fun unary(): Expr {
        if (match(TokenType.BANG, TokenType.MINUS)) {
            val op = previous()
            val right = unary()
            return Expr.Unary(op, right)
        }
        return primary()
    }

    private fun primary(): Expr {
        if (match(TokenType.FALSE)) return Expr.Literal(false)
        if (match(TokenType.TRUE)) return Expr.Literal(true)
        if (match(TokenType.NIL)) return Expr.Literal(null)

        if (match(TokenType.NUM, TokenType.STRING)) return Expr.Literal(previous().literal)

        if (match(TokenType.LPAREN)) {
            val inside = expression()
            consume(TokenType.RPAREN, "Mismatched ')'")
            return Expr.Grouping(inside)
        }

        // TODO: ERROR PRODUCTION chapter 06.

        reportError(peek(), "Expected expression")
        throw ParseError()
    }

    fun parse(): Expr? {
        return try {
            expression()
        } catch (e: ParseError) {
            null
        }
    }

    // Error handle
    private fun consume(type: TokenType, message: String): Token {
        if (check(type)) return advance()
        reportError(peek(), message)
        throw ParseError()
    }

    private fun reportError(token: Token, message: String) {
        if (token.type == TokenType.EOF) {
            reportError(token.line, " at end", message);
        } else {
            reportError(token.line, " at '${token.lexeme}'", message);
        }
    }

    private fun synchronize() {
        advance()

        while (!isAtEnd()) {
            if (previous().type == TokenType.SEMI) return
            when(peek().type) {
                TokenType.CLASS, TokenType.FUN, TokenType.VAR, TokenType.FOR, TokenType.IF, TokenType.WHILE, TokenType.PRINT, TokenType.RETURN -> return
                else -> advance()
            }
        }
    }
}

class ParseError: RuntimeException()