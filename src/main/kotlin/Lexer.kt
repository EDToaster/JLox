enum class TokenType {
    LPAREN, RPAREN, LBRACE, RBRACE,
    COMMA, DOT, MINUS, PLUS, SEMI, SLASH, STAR,


    BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL,
    GT, GEQ, LT, LEQ,

    // TERNARY
    QUESTION, COLON,

    IDENT, STRING, NUM,

    // KEYWORDS
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    // CONTROL FLOW KEYWORDS
    BREAK,

    EOF;

    companion object {
        fun keywordOf(string: String): TokenType? {
            return when (string) {
                "and" -> AND
                "or" -> OR
                "class" -> CLASS
                "fun" -> FUN
                "for" -> FOR
                "while" -> WHILE
                "if" -> IF
                "else" -> ELSE
                "true" -> TRUE
                "false" -> FALSE
                "nil" -> NIL
                "return" -> RETURN
                "super" -> SUPER
                "this" -> THIS
                "var" -> VAR
                "break" -> BREAK
                else -> null
            }
        }
    }
}

class Token(val type: TokenType, val lexeme: String, val literal: Any?, val line: Int) {

    override fun toString(): String {
        return "$type $lexeme $literal"
    }
}

class Lexer(private val source: String, private val reportError: (Int, String) -> Unit) {

    val tokens = mutableListOf<Token>()

    private var start = 0
    private var current = 0
    private var line = 1

    private fun isAtEnd(ahead: Int = 0): Boolean {
        return current + ahead >= source.length
    }

    private fun advance(): Char {
        val c = source[current++]

        if (c == '\n') line++

        return c
    }

    private fun match(expected: Char): Boolean {
        val next = source.elementAtOrNull(current) ?: return false
        if (next != expected) return false

        advance()
        return true
    }

    private fun peek(): Char? {
        return peek(0)
    }

    private fun peek(offset: Int): Char? {
        return source.elementAtOrNull(current + offset)
    }

    private fun peekRange(range: IntRange): String {
        val start = (current + range.first).coerceAtMost(source.length - 1)
        val end = (current + range.last).coerceAtMost(source.length - 1)
        return source.substring(start..end)
    }

    private fun currString(): String {
        return source.substring(start..<current)
    }

    private fun addToken(type: TokenType) {
        addToken(type, null)
    }

    private fun addToken(type: TokenType, literal: Any?) {
        tokens.add(Token(type, currString(), literal, line))
    }

    private fun scanString() {
        while (peek() != '"' && !isAtEnd()) {
            advance()
        }

        if (isAtEnd()) {
            reportError(line, "Unterminated string")
            return
        }

        advance()

        val value = source.substring(start + 1..< current - 1 )
        addToken(TokenType.STRING, value)
    }

    private fun scanNumber() {
        while (peek()?.isDigit() == true) advance()

        if (peek() == '.' && peek(1)?.isDigit() == true) {
            // consume dot
            advance()
            while (peek()?.isDigit() == true) advance()
        }

        addToken(TokenType.NUM, currString().toDouble())
    }

    private fun scanIdentifier() {
        // Identifiers are [_a-zA-Z][_0-9a-zA-Z]*
        while (peek()?.isJavaIdentifierPart() == true) advance()

        addToken(TokenType.keywordOf(currString()) ?: TokenType.IDENT)
    }

    /**
     * Scans a block comment assuming the starting delimiters have already been consumed.
     */
    private fun scanBlockComment() {
        var depth = 1

        while (depth > 0 && !isAtEnd(1)) {
            if (match('/') && match('*')) depth++
            else if (match('*') && match('/')) depth--
            else advance()
        }

        if (depth > 0) {
            reportError(line, "Unclosed comment")
        }
    }

    private fun scanToken() {
        when(val c = advance()) {
            '(' -> addToken(TokenType.LPAREN)
            ')' -> addToken(TokenType.RPAREN)
            '{' -> addToken(TokenType.LBRACE)
            '}' -> addToken(TokenType.RBRACE)
            ',' -> addToken(TokenType.COMMA)
            '.' -> addToken(TokenType.DOT)
            '-' -> addToken(TokenType.MINUS)
            '+' -> addToken(TokenType.PLUS)
            ';' -> addToken(TokenType.SEMI)
            '*' -> addToken(TokenType.STAR)
            '?' -> addToken(TokenType.QUESTION)
            ':' -> addToken(TokenType.COLON)
            '!' -> addToken(if (match('=')) TokenType.BANG_EQUAL else TokenType.BANG)
            '=' -> addToken(if (match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL)
            '<' -> addToken(if (match('=')) TokenType.LEQ else TokenType.LT)
            '>' -> addToken(if (match('=')) TokenType.GEQ else TokenType.GT)
            '/' -> if (match('/')) {
                // comment
                while (peek() != '\n' && !isAtEnd()) advance()
            } else if (match('*')) {
                scanBlockComment()
            } else {
                addToken(TokenType.SLASH)
            }
            ' ', '\r', '\n', '\t' -> {}
            '"' -> scanString()
            else -> if (c.isDigit()) {
                scanNumber()
            } else if (c.isJavaIdentifierStart()) {
                // Scan identifier/keywords
                scanIdentifier()
            } else {
                reportError(line, "Unexpected character $c")
            }
        }
    }

    fun scanTokens() {
        while (!isAtEnd()) {
            start = current
            scanToken()
        }

        tokens.add(Token(TokenType.EOF, "", null, line))
    }

}