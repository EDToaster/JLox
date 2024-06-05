import java.io.BufferedReader
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import kotlin.system.exitProcess

class Lox {

    private val environment = Environment()

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

    private val interpreter = Interpreter(environment)
    private val resolver = Resolver(interpreter.locals::put, environment.boundNames())
    private val astPrinter = ASTPrinter()

    // Runs the following prog, may be an incomplete fragment
    private fun run(reader: BufferedReader, showPrompt: Boolean = false) {

        lateinit var parser: Parser

        val iter: Iterator<Char> = if (showPrompt) {
            sequence {
                while (true) {
                    val ctx = parser.context

                    val prompt = when(ctx) {
                        Parser.Context.None -> "> "
                        Parser.Context.If -> "if ... "
                        Parser.Context.ThenBody -> "then ... "
                        Parser.Context.ElseBody -> "else ... "
                        Parser.Context.Block -> "block ... "
                    }

                    print(prompt)

                    val line = reader.readLine() ?: break
                    yieldAll(line.iterator())
                    yield('\n')
                }
            }.iterator()
        } else {
            reader.iter()
        }


        val lexer = Lexer(iter)
        parser = Parser(lexer.scanTokens().iterator())

        try {
            for (it in parser.parseProgram()) {
                // TODO: CHALLENGES chapter 11.
                resolver.resolve(it)
                interpreter.interpret(it)
            }
        } catch (e: InterpreterError) {
            println("${e.message}\n[line ${e.token.line}]")
        }
    }

    fun runFile(fileName: String): Int {
        val bytes = Files.readAllBytes(Paths.get(fileName))
        run(bytes.toString(Charset.defaultCharset()).reader().buffered())
        return 0
    }

    fun runPrompt(): Int {
        val underlying = InputStreamReader(System.`in`).buffered()
        run(underlying, showPrompt = true)
        return 0
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