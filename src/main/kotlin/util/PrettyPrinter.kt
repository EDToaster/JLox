package util

class PrettyPrinter(private val tabSize: Int = 4) {

    private val buffer: StringBuilder = StringBuilder()
    private val line: StringBuilder = StringBuilder()

    private var indentLevel: Int = 0

    fun indent(inside: PrettyPrinter.() -> Unit) {
        indentLevel += 1
        this.inside()
        indentLevel -= 1
    }

    fun append(s: String): PrettyPrinter {
        line.append(s)
        return this
    }

    fun finishLine(): PrettyPrinter {
        buffer.append("${" ".repeat(tabSize * indentLevel)}${this.line}").appendLine()
        line.clear()
        return this
    }

    override fun toString(): String = buffer.toString()
}