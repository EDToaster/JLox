import java.io.BufferedReader
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import kotlin.system.exitProcess

class Lox {

    val interpreter = Interpreter()
    val astPrinter = ASTPrinter()

    var runtimeErrored = false
    var errored = false

    // Runs the following prog, may be an incomplete fragment
    private fun run(prog: String) {
        val lexer = Lexer(prog, ::reportError)
        lexer.scanTokens()
        val parser = Parser(lexer.tokens, ::report)
        val exprs = parser.parse() ?: return

        try {
            exprs.forEach { interpreter.interpret(it) }
        } catch (e: InterpreterError) {
            reportInterpreterError(e)
            runtimeErrored = true
        }
    }

    fun runFile(fileName: String): Int {
        val bytes = Files.readAllBytes(Paths.get(fileName))
        run(bytes.toString(Charset.defaultCharset()))
        return if (errored) 65 else if (runtimeErrored) 70 else 0
    }

    fun runPrompt(): Int {
        BufferedReader(InputStreamReader(System.`in`)).use {
            while (true) {
                print("> ")
                run(it.readLine() ?: break)
                errored = false
            }
        }
        return 0
    }

    fun reportError(line: Int, message: String) {
        report(line, "", message)
    }

    fun reportInterpreterError(error: InterpreterError) {
        println("${error.message}\n[line ${error.token.line}]")
    }

    private fun report(line: Int, where: String, message: String) {
        println("[line $line] Error$where: $message")
        errored = true
    }
}


fun usage() {
    println("Usage: jlox [script]")
    exitProcess(1)
}

fun main(args: Array<String>) {
    exitProcess(
        if (args.size > 1) {
            usage()
            1
        } else if (args.size == 1) {
            Lox().runFile(args[0])
        } else {
            Lox().runPrompt()
        }
    )
}