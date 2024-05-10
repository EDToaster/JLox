import org.junit.jupiter.api.Test

class ASTPrinterTest {

    @Test
    fun `Test Pretty Print`() {
        val expr = Expr.Binary(
            Token(TokenType.STAR, "*", null, 1),
            Expr.Unary(Token(TokenType.MINUS, "-", null, 1), Expr.Literal(123)),
            Expr.Grouping(Expr.Literal(45.67))
        )
        println(ASTPrinter().visit(expr))
    }
}