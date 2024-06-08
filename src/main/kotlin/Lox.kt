import util.PrettyPrinter
import java.io.BufferedReader
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import kotlin.math.roundToInt
import kotlin.system.exitProcess

class Lox {

    private val environment = Environment()

    private fun Environment.declareNativeFun(name: String, arity: Int?, callable: (List<Any?>) -> Any?) {
        this.declare(name, object: NativeLoxFunction(name, arity) {
            override fun call(args: List<Any?>): Any? = callable(args)
        })
    }

    init {
        environment.run {
            declareNativeFun("print", null) { args ->
                println(args.joinToString(" ", transform = Any?::toJLoxString))
                null
            }

            declareNativeFun("time", 0) {
                System.currentTimeMillis().toDouble()
            }

            declareNativeFun("assert", 1) {
                if (!truthy(it[0])) throw AssertionException()
                it[0]
            }

            declareNativeFun("exit", 1) {
                throw ExitException((it[0] as Double).roundToInt())
            }
        }
    }

    private val interpreter = Interpreter(environment)
    private val resolver = Resolver(interpreter.locals::put, environment.boundNames())

    // Runs the following prog, may be an incomplete fragment
    private fun run(reader: BufferedReader, showPrompt: Boolean = false): Int {

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

//        val prettyPrinter = PrettyPrinter(2)
//        val astPrinter = ASTPrinter(prettyPrinter)

        try {
            for (it in parser.parseProgram()) {
//                it.accept(astPrinter)
                // TODO: CHALLENGES chapter 11.
                resolver.resolve(it)
                interpreter.interpret(it)
            }
        } catch (e: InterpreterError) {
            println("[line ${e.token.line}] ${e.message}")
            return 11
        } catch (e: ParseError) {
            println("[line ${e.line}] ${e.message}")
            return 10
        } catch (e: AssertionError) {
            println("Failed assertion!")
            return 1
        } catch (e: ExitException) {
            return e.retCode
        }

//        println(prettyPrinter)

        return 0
    }

    fun runFile(fileName: String): Int {
        val bytes = Files.readAllBytes(Paths.get(fileName))
        return run(bytes.toString(Charset.defaultCharset()).reader().buffered())
    }

    fun runPrompt(): Int {
        val underlying = InputStreamReader(System.`in`).buffered()
        return run(underlying, showPrompt = true)
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