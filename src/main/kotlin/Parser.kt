import ast.Expr
import ast.MatchClause
import ast.Stmt
import util.PrettyPrinter

inline fun <T> Iterable<T>.joinForEach(insideAction: () -> Unit, action: (T) -> Unit): Unit {
    var first = true
    for (element in this) {
        if (!first) {
            insideAction()
        }
        action(element)
        first = false
    }
}


class ASTPrinter(val pp: PrettyPrinter): Expr.Visitor<Unit>, Stmt.Visitor<Unit> {
    override fun visit(binary: Expr.Binary) {
        binary.left.accept(this)
        pp.append(" ${binary.operator.lexeme} ")
        binary.right.accept(this)
    }

    override fun visit(grouping: Expr.Grouping) {
        pp.append("(${grouping.expr.accept(this)})")
    }
    override fun visit(literal: Expr.Literal) {
        pp.append("${literal.value ?: "nil"}")
    }

    override fun visit(literal: Expr.StringLiteral) {
        pp.append("\"")
        for ((prefix, expr) in literal.prefixes) {
            pp.append(prefix)
            pp.append("\${")
            expr.accept(this)
            pp.append("}")
        }
        pp.append(literal.rest)
        pp.append("\"")
    }

    override fun visit(variable: Expr.Variable) {
        pp.append(variable.name.lexeme)
    }

    override fun visit(unary: Expr.Unary) {
        pp.append(unary.operator.lexeme)
        unary.right.accept(this)
    }

    override fun visit(ternary: Expr.Ternary) {
        ternary.cond.accept(this)
        pp.append(" ? ")
        ternary.t.accept(this)
        pp.append(" : ")
        ternary.e.accept(this)
    }

    override fun visit(assign: Expr.Assign) {
        pp.append("${assign.name.lexeme} = ")
        assign.value.accept(this)
    }

    override fun visit(call: Expr.Call) {
        call.callee.accept(this)
        pp.append("(")

        call.arguments.joinForEach({ pp.append(", ") }) { it.accept(this) }
        pp.append(")")
    }


    override fun visit(get: Expr.Get) {
        get.obj.accept(this)
        pp.append(".${get.name.lexeme}")
    }

    override fun visit(set: Expr.Set) {
        set.obj.accept(this)
        pp.append(".${set.name.lexeme} = ")
        set.value.accept(this)
    }

    override fun visit(funDef: Expr.FunDef) {
        pp.append("fun ${funDef.name?.lexeme ?: ""}(")
        funDef.params.joinForEach({ pp.append(", ") }) { pp.append(it.lexeme) }
        pp.append(") ")
        funDef.body.accept(this)
    }

    override fun visit(t: Expr.This) {
        pp.append("this")
    }

    override fun visit(expression: Stmt.Expression) {
        expression.expression.accept(this)
        pp.append(";").finishLine()
    }

    override fun visit(decl: Stmt.Decl) {
        pp.append("var ${decl.name.lexeme}")
        if (decl.init != null) {
            pp.append(" = ")
            decl.init.accept(this)
        }

        pp.append(";").finishLine()
    }

    override fun visit(funDecl: Stmt.FunDecl) {
        funDecl.body.accept(this)
    }

    override fun visit(classDecl: Stmt.ClassDecl): Unit = pp.run {
        append("class ${classDecl.name.lexeme} {").finishLine()
        indent {
            classDecl.methods.forEach { it.accept(this@ASTPrinter) }
        }
        append("}").finishLine()
    }

    override fun visit(block: Stmt.Block): Unit = pp.run {
        append("{").finishLine()
        indent {
            block.body.forEach { it.accept(this@ASTPrinter) }
        }
        append("}").finishLine()
    }

    override fun visit(ifStmt: Stmt.IfStmt): Unit = pp.run {
        append("if (")
        ifStmt.cond.accept(this@ASTPrinter)
        append(") ")
        ifStmt.t.accept(this@ASTPrinter)
        if (ifStmt.e != null) {
            pp.append("else ")
            ifStmt.e.accept(this@ASTPrinter)
        }
        finishLine()
    }

    override fun visit(whileStmt: Stmt.WhileStmt): Unit = pp.run {
        append("while (")
        whileStmt.cond.accept(this@ASTPrinter)
        append(") ")
        if (whileStmt.body != null) {
            whileStmt.body.accept(this@ASTPrinter)
        } else {
            append(";").finishLine()
        }
    }

    override fun visit(matchStmt: Stmt.MatchStmt): Unit = pp.run {
        append("match ...").finishLine()
    }

    override fun visit(breakStmt: Stmt.Break) {
        pp.append("break;").finishLine()
    }

    override fun visit(retStmt: Stmt.Return): Unit = pp.run {
        pp.append("return")
        if (retStmt.value != null) {
            pp.append(" ")
            retStmt.value.accept(this@ASTPrinter)
        }
        pp.append(";").finishLine()
    }
}

class Parser(tokens: Iterator<Token>) {

    enum class Context {
        None,
        If,
        ThenBody,
        ElseBody,
        Block,
    }

    var context = Context.None
        private set

    private fun <T> withContext(context: Context, block: () -> T): T{
        val prevContext = this.context
        this.context = context
        val ret = block()
        this.context = prevContext
        return ret
    }

    private val tokens: PeekingIterator<Token> = tokens.peeking()
    private lateinit var previousToken: Token

    private fun isAtEnd(): Boolean = peek().type == TokenType.EOF
    private fun peek(): Token = tokens.peek()
    private fun previous(): Token = previousToken
    private fun advance(): Token {
        val next = tokens.next()
        previousToken = next
        return next
    }

    private fun check(type: TokenType): Boolean {
        return !isAtEnd() && peek().type == type
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
     * declaration    → classDecl
     *                | varDecl
     *                | funDef
     *                | statement
     *
     * classDecl      → "class" IDENT "{" funRest* "}" ;
     *
     * varDecl        → "var" IDENT ( "=" expression )? ";" ;
     *
     * funDef         → "fun" funRest ;
     * funRest        → IDENT "(" params? ")" statement ;
     *
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

    fun parseProgram() = sequence {
        while (!isAtEnd()) {
            yield(declaration())
        }
    }

    private fun declaration(): Stmt {
        if (match(TokenType.CLASS)) {
            val ident = consume(TokenType.IDENT, "Expected variable name after 'class'")
            consume(TokenType.LBRACE, "Expected '{' after class identifier")

            val methods = mutableListOf<Stmt.FunDecl>()
            while (!check(TokenType.RBRACE) && !isAtEnd()) {
                methods.add(finishFunStmt())
            }

            consume(TokenType.RBRACE, "Expected '}' after class definition")
            return Stmt.ClassDecl(ident, methods)
        }

        if (match(TokenType.VAR)) {
            val ident = consume(TokenType.IDENT, "Expected variable name after 'var'")
            val init = if (match(TokenType.EQUAL)) expression() else null
            consume(TokenType.SEMI, "Expected ';' after variable declaration")
            return Stmt.Decl(ident, init)
        }

        if (match(TokenType.FUN)) return finishFunStmt()

        return statement()
    }

    private fun finishFunStmt(): Stmt.FunDecl {
        val ident = consume(TokenType.IDENT, "Expected function name after 'fun'")
        return Stmt.FunDecl(ident, finishFunExpr(name = ident))
    }

    private fun statement(): Stmt {
        return if (match(TokenType.LBRACE)) Stmt.Block(block())
        else if (match(TokenType.IF)) ifStmt()
        else if (match(TokenType.WHILE)) whileStmt()
        else if (match(TokenType.MATCH)) matchStmt()
        else if (match(TokenType.FOR)) forStmt()
        else if (match(TokenType.BREAK)) breakStmt()
        else if (match(TokenType.RETURN)) returnStmt()
        else Stmt.Expression(semiStmt())
    }

    private fun block(): List<Stmt> = withContext(Context.Block) {
        val stmts = mutableListOf<Stmt>()
        while (!check(TokenType.RBRACE) && !isAtEnd()) {
            stmts.add(declaration())
        }

        consume(TokenType.RBRACE, "Expected '}' after a block")
        stmts
    }

    private fun ifStmt(): Stmt = withContext(Context.If) {
        consume(TokenType.LPAREN, "Expected '(' after 'if'")
        val cond = expression()
        consume(TokenType.RPAREN, "Expected ')' after if")

        val thenBranch = withContext(Context.ThenBody) { statement() }
        val elseBranch = if (match(TokenType.ELSE)) withContext(Context.ElseBody) { statement() } else null

        Stmt.IfStmt(cond, thenBranch, elseBranch)
    }

    private fun whileStmt(): Stmt {
        consume(TokenType.LPAREN, "Expected '(' after 'while'")
        val cond = expression()
        consume(TokenType.RPAREN, "Expected ')' after 'while'")

        val body = if (match(TokenType.SEMI)) null else statement()
        return Stmt.WhileStmt(cond, body)
    }

    private fun matchClause(): MatchClause {
        // Bar separated expressions
        val exprs = mutableListOf<Expr>()
        val isElse = match(TokenType.ELSE)

        if (!isElse) {
            do {
                exprs.add(assignment())
            } while (match(TokenType.COMMA))
        }

        consume(TokenType.FAT_ARROW, "Expected '=>' after match condition")

        val stmt = statement()
        return MatchClause(isElse, exprs, stmt)
    }

    private fun matchStmt(): Stmt {
        consume(TokenType.LPAREN, "Expected '(' after 'match'")
        val obj = expression()
        consume(TokenType.RPAREN, "Expected ')' after 'match'")

        consume(TokenType.LBRACE, "Expected '{' before match clauses")

        val clauses = mutableListOf<MatchClause>()

        while (!match(TokenType.RBRACE)) {
            // find match clause
            clauses.add(matchClause())
        }

        return Stmt.MatchStmt(obj, clauses)
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
        return Stmt.Break
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
     * assignment     → ( call "." )? ( IDENT '=' expression )
     *                | andor ;
     * andor          → ternary ( ( "or" | "and" ) ternary)*
     * ternary        → equality ( "?" expression ":" ternary )? ;
     * equality       → comparison ( ( "!=" | "==" ) comparison )* ;
     * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
     * term           → factor ( ( "-" | "+" ) factor )* ;
     * factor         → unary ( ( "/" | "*" ) unary )* ;
     * unary          → ( "!" | "-" ) unary
     *                | call ;
     * call           → funExpr ( "(" arguments? ")" | "." IDENT )* ;
     * arguments      → expression ( "," expression )* ;
     *
     * funExpr        → "fun" "(" params? ")" statement
     *                | primary ;
     *
     * primary        → NUMBER | STRING | "true" | "false" | "nil"
     *                | THIS
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
            } else if (expr is Expr.Get) {
                return Expr.Set(expr.obj, expr.name, value)
            }

            throw ParseError(equals.line, "Invalid l-value")
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
                consume(TokenType.RPAREN, "Expected ')' after arguments");
                expr = Expr.Call(expr, lparen, args)
            } else if(match(TokenType.DOT)) {
                expr = Expr.Get(expr, consume(TokenType.IDENT, "Expected property name after '.'"))
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

        if (match(TokenType.NUM)) return Expr.Literal(previous().literal)

        if (match(TokenType.STRING_INTERP, TokenType.STRING)) return stringInterp()

        if (match(TokenType.THIS)) return Expr.This(previous())
        if (match(TokenType.IDENT)) return Expr.Variable(previous())

        if (match(TokenType.LPAREN)) {
            val inside = expression()
            consume(TokenType.RPAREN, "Mismatched ')'")
            return Expr.Grouping(inside)
        }

        // TODO: ERROR PRODUCTION chapter 06.

        throw ParseError(peek().line, "Expected expression")
    }

    private fun stringInterp(): Expr {
        val pairs = mutableListOf<Pair<String, Expr>>()

        while (true) {
            when (previous().type) {
                TokenType.STRING -> {
                    return Expr.StringLiteral(pairs, previous().literal as String)
                }
                TokenType.STRING_INTERP -> {
                    val prev = previous()
                    val expr = expression()
                    consume(TokenType.RBRACE, "Expected '}' after string interpolation expression'")
                    pairs.add(prev.literal as String to expr)
                }
                else -> {
                    throw ParseError(previous().line, "Malformed string interpolation expression")
                }
            }

            advance()
        }
    }

    // Error handle
    private fun consume(type: TokenType, message: String): Token {
        if (check(type)) return advance()
        throw ParseError(peek().line, message)
    }
}

class ParseError(val line: Int, message: String) : RuntimeException(message)