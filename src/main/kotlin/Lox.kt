import java.io.BufferedReader
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import kotlin.system.exitProcess

class Lox {

    val environment = Environment()

    init {
        environment.run {
            declare("print", object: LoxCallable {
                override fun call(args: List<Any?>): Any? {
                    println(args.joinToString(" ", transform = Any?::toJLoxString))
                    return null
                }
                override fun arity(): Int? = null
                override fun toString(): String = "<native fun print:#${arity()}>"
            })

            declare("time", object: LoxCallable {
                override fun call(args: List<Any?>): Any = System.currentTimeMillis().toDouble()
                override fun arity(): Int = 0
                override fun toString(): String = "<native fun time:#${arity()}>"
            })

            declare("assert", object: LoxCallable {
                override fun call(args: List<Any?>): Any? {
                    if (!truthy(args[0])) {
                        throw AssertionException()
                    }
                    return null
                }

                override fun arity(): Int = 1
            })
        }
    }

    val interpreter = Interpreter(environment)
    val resolver = Resolver(interpreter.locals::put, environment.boundNames())
    val astPrinter = ASTPrinter()

    var runtimeErrored = false
    var errored = false

    // Runs the following prog, may be an incomplete fragment
    private fun run(prog: String) {
        val lexer = Lexer(prog, ::reportError)
        lexer.scanTokens()
        val parser = Parser(lexer.tokens, ::report)
        val stmts = parser.parse() ?: return

        try {
            stmts.forEach {
                // TODO: CHALLENGERS chapter 11.
                resolver.resolve(it)
                if (errored) return@forEach
                interpreter.interpret(it)
            }
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