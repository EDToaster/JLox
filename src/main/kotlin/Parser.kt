sealed class Expr {
    class Binary(val operator: Token, val left: Expr, val right: Expr): Expr()
    class Grouping(val expr: Expr): Expr()
    class Literal(val value: Any?): Expr()
    class Variable(val name: Token): Expr()
    class Unary(val operator: Token, val right: Expr): Expr()
    class Ternary(val cond: Expr, val t: Expr, val e: Expr): Expr()
    class Assign(val name: Token, val value: Expr): Expr()
    class Call(val callee: Expr, val lparen: Token, val arguments: List<Expr>): Expr()
    class FunDef(val name: Token?, val params: List<Token>, val body: Stmt): Expr()

    fun <R> accept(visitor: Visitor<R>): R = when (this) {
        is Binary -> visitor.visit(this)
        is Grouping -> visitor.visit(this)
        is Literal -> visitor.visit(this)
        is Variable -> visitor.visit(this)
        is Unary -> visitor.visit(this)
        is Ternary -> visitor.visit(this)
        is Assign -> visitor.visit(this)
        is Call -> visitor.visit(this)
        is FunDef -> visitor.visit(this)
    }

    interface Visitor<R> {
        fun visit(binary: Binary): R
        fun visit(grouping: Grouping): R
        fun visit(literal: Literal): R
        fun visit(variable: Variable): R
        fun visit(unary: Unary): R
        fun visit(ternary: Ternary): R
        fun visit(assign: Assign): R
        fun visit(call: Call): R
        fun visit(funDef: FunDef): R
    }
}

class ASTPrinter: Expr.Visitor<String>, Stmt.Visitor<String> {
    override fun visit(binary: Expr.Binary): String =
        "(${binary.operator.lexeme} ${binary.left.accept(this)} ${binary.right.accept(this)})"

    override fun visit(grouping: Expr.Grouping): String =
        "(group ${grouping.expr.accept(this)})"

    override fun visit(literal: Expr.Literal): String =
        "${literal.value ?: "nil"}"

    override fun visit(variable: Expr.Variable): String = variable.name.lexeme

    override fun visit(unary: Expr.Unary): String =
        "(${unary.operator.lexeme} ${unary.right.accept(this)})"

    override fun visit(ternary: Expr.Ternary): String =
        "(${ternary.cond.accept(this)} ? ${ternary.t.accept(this)} : ${ternary.e.accept(this)})"

    override fun visit(assign: Expr.Assign): String =
        "${assign.name.lexeme} ${assign.value.accept(this)}"

    override fun visit(call: Expr.Call): String {
        TODO()
    }

    override fun visit(funDef: Expr.FunDef): String {
        TODO("Not yet implemented")
    }

    override fun visit(expression: Stmt.Expression): String =
        "${expression.expression.accept(this)} ;"

    override fun visit(decl: Stmt.Decl): String =
        "var ${decl.name.lexeme} = ${decl.init?.accept(this) ?: "no init"} ;"

    override fun visit(block: Stmt.Block): String =
        "{ ${block.body.joinToString(" ") { it.accept(this) }} }"

    override fun visit(ifStmt: Stmt.IfStmt): String =
        "if ( ${ifStmt.cond.accept(this)} ) ${ifStmt.t.accept(this)} else ${ifStmt.e?.accept(this) ?: "no else"}"

    override fun visit(whileStmt: Stmt.WhileStmt): String =
        "while ( ${whileStmt.cond.accept(this)} ) ${whileStmt.body?.accept(this) ?: "no body"}"

    override fun visit(breakStmt: Stmt.Break): String =
        "break ;"

    override fun visit(retStmt: Stmt.Return): String {
        TODO("Not yet implemented")
    }

    override fun visit(funDecl: Stmt.FunDecl): String {
        TODO("Not yet implemented")
    }
}

sealed class Stmt {
    class Expression(val expression: Expr): Stmt()
    class Decl(val name: Token, val init: Expr?): Stmt()
    class FunDecl(val name: Token, val body: Expr.FunDef): Stmt()
    class Block(val body: List<Stmt>): Stmt()
    class IfStmt(val cond: Expr, val t: Stmt, val e: Stmt?): Stmt()
    class WhileStmt(val cond: Expr, val body: Stmt?): Stmt()
    class Break: Stmt()
    class Return(val value: Expr?): Stmt()

    fun <R> accept(visitor: Visitor<R>): R = when (this) {
        is Expression -> visitor.visit(this)
        is Decl -> visitor.visit(this)
        is Block -> visitor.visit(this)
        is IfStmt -> visitor.visit(this)
        is WhileStmt -> visitor.visit(this)
        is Break -> visitor.visit(this)
        is Return -> visitor.visit(this)
        is FunDecl -> visitor.visit(this)
    }

    interface Visitor<R> {
        fun visit(expression: Expression): R
        fun visit(decl: Decl): R
        fun visit(block: Block): R
        fun visit(ifStmt: IfStmt): R
        fun visit(whileStmt: WhileStmt): R
        fun visit(breakStmt: Break): R
        fun visit(retStmt: Return): R
        fun visit(funDecl: FunDecl): R
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
     * program        → declaration* EOF ;
     *
     * declaration    → varDecl
     *                | funDef
     *                | statement
     *
     * varDecl        → "var" IDENT ( "=" expression )? ";" ;
     *
     * funDef         → "fun" IDENT "(" params? ")" statement;
     * params         → IDENT ( "," IDENT )* ;
     *
     * statement      → exprStmt
     *                | ifStmt
     *                | whileStmt
     *                | forStmt
     *                | printStmt
     *                | break
     *                | return
     *                | block ;
     *
     * whileStmt      → "while" "(" expression ")" ( ";" | statement ) ;
     *
     * forStmt        → "for" "(" statement? ";" expression? ";" statement? ")" ( ";" | statement ) ;
     *
     * ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
     *
     * break          → "break" ";" ;
     *
     * return         → "return" expression? ";" ;
     *
     * block          → "{" declaration* "}"
     *
     * exprStmt       → expression ";" ;
     * printStmt      → "print" expression ";" ;
     */

    private fun program(): List<Stmt> {
        val stmts = mutableListOf<Stmt>()
        while (!isAtEnd()) {
            try {
                stmts.add(declaration())
            } catch (e: ParseError) {
                synchronize()
            }
        }
        return stmts
    }

    private fun declaration(): Stmt {
        if (match(TokenType.VAR)) {
            val ident = consume(TokenType.IDENT, "Expected variable name after 'var'")
            val init = if (match(TokenType.EQUAL)) expression() else null
            consume(TokenType.SEMI, "Expected ';' after variable declaration")
            return Stmt.Decl(ident, init)
        }

        if (match(TokenType.FUN)) {
            val ident = consume(TokenType.IDENT, "Expected function name after 'fun'")
            return Stmt.FunDecl(ident, finishFunExpr(name = ident))
        }

        return statement()
    }

    private fun statement(): Stmt {
        return if (match(TokenType.LBRACE)) Stmt.Block(block())
        else if (match(TokenType.IF)) ifStmt()
        else if (match(TokenType.WHILE)) whileStmt()
        else if (match(TokenType.FOR)) forStmt()
        else if (match(TokenType.BREAK)) breakStmt()
        else if (match(TokenType.RETURN)) returnStmt()
        else Stmt.Expression(semiStmt())
    }

    private fun block(): List<Stmt> {
        val stmts = mutableListOf<Stmt>()
        while (!check(TokenType.RBRACE) && !isAtEnd()) {
            stmts.add(declaration())
        }

        consume(TokenType.RBRACE, "Expected '}' after a block")
        return stmts
    }

    private fun ifStmt(): Stmt {
        consume(TokenType.LPAREN, "Expected '(' after 'if'")
        val cond = expression()
        consume(TokenType.RPAREN, "Expected ')' after if")

        val thenBranch = statement()
        val elseBranch = if (match(TokenType.ELSE)) statement() else null

        return Stmt.IfStmt(cond, thenBranch, elseBranch)
    }

    private fun whileStmt(): Stmt {
        consume(TokenType.LPAREN, "Expected ')' after 'while'")
        val cond = expression()
        consume(TokenType.RPAREN, "Expected ')' after 'while'")

        val body = if (match(TokenType.SEMI)) null else statement()
        return Stmt.WhileStmt(cond, body)
    }

    private fun forStmt(): Stmt {
        consume(TokenType.LPAREN, "Expected '(' after 'for'")
        val init = if (match(TokenType.SEMI)) null else declaration()
        val cond = if (match(TokenType.SEMI)) {
            Expr.Literal(true)
        } else {
            val stmt = expression()
            consume(TokenType.SEMI, "Expected ';' after condition")
            stmt
        }

        val post = if (match(TokenType.RPAREN)) null else {
            val expr = expression()
            consume(TokenType.RPAREN, "Expected ')' after post expression")
            expr
        }

        val body = if (match(TokenType.SEMI)) null else statement()

        val innerBlock = mutableListOf<Stmt>()
        body?.let { innerBlock.add(it) }
        post?.let { innerBlock.add(Stmt.Expression(it)) }

        val innerWhile = Stmt.WhileStmt(cond, Stmt.Block(innerBlock))
        val outerBlock = mutableListOf<Stmt>()
        init?.let { outerBlock.add(it) }
        outerBlock.add(innerWhile)

        val ret = Stmt.Block(outerBlock)
        return ret
    }

    private fun breakStmt(): Stmt {
        consume(TokenType.SEMI, "Expect ';' after 'break'")
        return Stmt.Break()
    }

    private fun returnStmt(): Stmt {
        val value = if (match(TokenType.SEMI)) null else {
            val expr = expression()
            consume(TokenType.SEMI, "Expect ';' after 'return'")
            expr
        }
        return Stmt.Return(value)
    }

    private fun semiStmt(): Expr {
        val value = expression()
        consume(TokenType.SEMI, "Expect ';' after statement")
        return value
    }

    /*
     * expression     → sequence ;
     * sequence       → assignment ( "," assignment )* ;
     * assignment     → (IDENT '=' expression)
     *                | andor ;
     * andor          → ternary ( ( "or" | "and" ) ternary)*
     * ternary        → equality ( "?" expression ":" ternary )? ;
     * equality       → comparison ( ( "!=" | "==" ) comparison )* ;
     * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
     * term           → factor ( ( "-" | "+" ) factor )* ;
     * factor         → unary ( ( "/" | "*" ) unary )* ;
     * unary          → ( "!" | "-" ) unary
     *                | call ;
     * call           → funExpr ( "(" arguments? ")" )* ;
     * arguments      → expression ( "," expression )* ;
     *
     * funExpr        → "fun" "(" params? ")" statement
     *                | primary ;
     *
     * primary        → NUMBER | STRING | "true" | "false" | "nil"
     *                | IDENT
     *                | "(" expression ")" ;
     *                # ERROR PRODUCTION RULES
     *                | ( "!=" | "==" ) equality
     *                | ( ">" | ">=" | "<" | "<=" ) comparison
     *                | "+" term
     *                | ( "/" | "*" ) factor
     */

    private fun expression(): Expr = sequence()
    private fun sequence(): Expr = binop(::assignment, TokenType.COMMA)

    private fun assignment(): Expr {
        // we don't know if expr is an l-value or r-value
        val expr = andor()
        if (match(TokenType.EQUAL)) {
            // left side should be l-value
            val equals = previous()
            val value = assignment()

            if (expr is Expr.Variable) {
                return Expr.Assign(expr.name, value)
            }

            reportError(equals, "Invalid l-value")
        }

        // is actually r-value
        return expr
    }

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
        return call()
    }

    private fun call(): Expr {
        var expr = funExpr()
        while (true) {
            if (match(TokenType.LPAREN)) {
                val lparen = previous()
                val args = mutableListOf<Expr>()
                if (!check(TokenType.RPAREN)) {
                    do {
                        args.add(assignment());
                    } while (match(TokenType.COMMA));
                }
                consume(TokenType.RPAREN, "Expect ')' after arguments.");
                expr = Expr.Call(expr, lparen, args)
            } else {
                break
            }
        }
        return expr
    }

    private fun funExpr(): Expr = if (match(TokenType.FUN)) {
        finishFunExpr()
    } else {
        primary()
    }

    private fun finishFunExpr(name: Token? = null): Expr.FunDef {
        consume(TokenType.LPAREN, "Expected '(' after function name'")

        val parameters = mutableListOf<Token>()
        if (!check(TokenType.RPAREN)) {
            do {
                parameters.add(consume(TokenType.IDENT, "Expected parameter name"))
            } while (match(TokenType.COMMA));
        }

        consume(TokenType.RPAREN, "Expected ')' after function parameter list")

        val body = statement()
        return Expr.FunDef(name, parameters, body)
    }

    private fun primary(): Expr {
        if (match(TokenType.FALSE)) return Expr.Literal(false)
        if (match(TokenType.TRUE)) return Expr.Literal(true)
        if (match(TokenType.NIL)) return Expr.Literal(null)

        if (match(TokenType.NUM, TokenType.STRING)) return Expr.Literal(previous().literal)

        if (match(TokenType.IDENT)) return Expr.Variable(previous())

        if (match(TokenType.LPAREN)) {
            val inside = expression()
            consume(TokenType.RPAREN, "Mismatched ')'")
            return Expr.Grouping(inside)
        }

        // TODO: ERROR PRODUCTION chapter 06.

        reportError(peek(), "Expected expression")
        throw ParseError()
    }

    fun parse(): List<Stmt>? = try {
            program()
        } catch (e: ParseError) {
            null
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
                TokenType.CLASS, TokenType.FUN, TokenType.VAR, TokenType.FOR, TokenType.IF, TokenType.WHILE, TokenType.RETURN -> return
                else -> advance()
            }
        }
    }
}

class ParseError: RuntimeException()