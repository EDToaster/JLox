import java.io.Reader

enum class TokenType {
    LPAREN, RPAREN, LBRACE, RBRACE,
    COMMA, DOT, MINUS, PLUS, SEMI, SLASH, STAR,


    BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL,
    GT, GEQ, LT, LEQ,

    // TERNARY
    QUESTION, COLON,

    IDENT, STRING, STRING_INTERP, NUM,

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

class Lexer(inputstream: Iterator<Char>) {

    private val currLexeme = StringBuilder()
    private val buffer = inputstream.peeking()

    // Keep track of the current line
    private var line = 1

    private fun isAtEnd() = !buffer.hasNext()

    private fun advance(): Char {
        val nextChar = buffer.next()
        currLexeme.append(nextChar)
        if (nextChar == '\n') line++
        return nextChar
    }

    private fun match(c: Char): Boolean = if (peek() == c) {
        advance()
        true
    } else {
        false
    }

    private fun peek(): Char = buffer.peek()

    private fun token(type: TokenType, literal: Any? = null, lstrip: Int = 0, rstrip: Int = 0): Token {
        val l = currLexeme.toString()
        return Token(type, l.substring(lstrip, l.length-rstrip), literal, line)
    }

    /**
     * Takes characters until seeing c
     */
    private fun takeUntil(c: Char, consumeMatch: Boolean = false) {
        takeUntil(c::equals, consumeMatch)
    }

    private fun takeUntil(pred: (Char) -> Boolean, consumeMatch: Boolean = false) {
        while (!isAtEnd() && !pred(peek())) advance()
        if (!isAtEnd() && consumeMatch) advance()
    }

    private fun takeWhile(pred: (Char) -> Boolean) {
        takeUntil({ !pred(it) })
    }

    private fun finishString(): Sequence<Token> = sequence {
        currLexeme.clear()

        // keep going until we hit ${ or "
        var dollar = false
        while (!isAtEnd()) {
            val c = advance()
            if (c == '"') {
                val literal = currLexeme.toString()
                yield(token(TokenType.STRING, literal = literal.substring(0, literal.length - 1), rstrip = 1))
                return@sequence
            } else if (dollar && c == '{') {
                // we have a string interp sequence!
                val literal = currLexeme.toString()
                yield(token(TokenType.STRING_INTERP, literal = literal.substring(0, literal.length - 2), rstrip = 2))
                yieldAll(scanTokens())
                // continue lexing string
                yieldAll(finishString())
                return@sequence
            } else if (c == '$') {
                dollar = true
            }
        }
    }

    private fun finishNumber(): Token {
        takeWhile(Char::isDigit)
        // optionally consume a '.'
        if (match('.')) {
            takeWhile(Char::isDigit)
        }

        return token(TokenType.NUM, literal = currLexeme.toString().toDouble())
    }

    private fun finishIdentifier(): Token {
        takeWhile(Char::isJavaIdentifierPart)

        return token(TokenType.keywordOf(currLexeme.toString()) ?: TokenType.IDENT)
    }


    private fun finishLineComment() {
        takeUntil('\n', consumeMatch = true)
    }

    /**
     * Scans a block comment assuming the starting delimiters have already been consumed.
     */
    private fun finishBlockComment() {
        var depth = 1

        while (depth > 0 && !isAtEnd()) {
            if (match('/') && match('*')) depth++
            else if (match('*') && match('/')) depth--
            else advance()
        }

        if (depth > 0) {
            throw LexerError(line, "Unclosed comment")
        }
    }

    fun scanTokens(): Sequence<Token> = sequence {
        while (!isAtEnd()) {
            currLexeme.clear()

            when (val c = advance()) {
                '(' -> yield(token(TokenType.LPAREN))
                ')' -> yield(token(TokenType.RPAREN))
                '{' -> {
                    yield(token(TokenType.LBRACE))
                    yieldAll(scanTokens())
                }
                '}' -> {
                    yield(token(TokenType.RBRACE))
                    return@sequence
                }
                ',' -> yield(token(TokenType.COMMA))
                '.' -> yield(token(TokenType.DOT))
                '-' -> yield(token(TokenType.MINUS))
                '+' -> yield(token(TokenType.PLUS))
                ';' -> yield(token(TokenType.SEMI))
                '*' -> yield(token(TokenType.STAR))
                '?' -> yield(token(TokenType.QUESTION))
                ':' -> yield(token(TokenType.COLON))
                '!' -> yield(token(if (match('=')) TokenType.BANG_EQUAL else TokenType.BANG))
                '=' -> yield(token(if (match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL))
                '<' -> yield(token(if (match('=')) TokenType.LEQ else TokenType.LT))
                '>' -> yield(token(if (match('=')) TokenType.GEQ else TokenType.GT))
                '/' -> if (match('/')) {
                    finishLineComment()
                } else if (match('*')) {
                    finishBlockComment()
                } else {
                    yield(token(TokenType.SLASH))
                }
                ' ', '\r', '\n', '\t' -> {}
                '"' -> yieldAll(finishString())
                else -> if (c.isDigit()) {
                    yield(finishNumber())
                } else if (c.isJavaIdentifierStart()) {
                    // Scan identifier/keywords
                    yield(finishIdentifier())
                } else {
                    throw LexerError(line, "Unexpected character $c")
                }
            }
        }

        yield(Token(TokenType.EOF, "", null, line))
    }

}

class LexerError(val line: Int, message: String) : RuntimeException(message)