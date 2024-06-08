interface LoxCallable {
    fun call(args: List<Any?>): Any?
    // returns null for vararg
    fun arity(): Int?

    fun bind(obj: LoxObject): LoxCallable = this
}

abstract class NativeLoxFunction(val name: String, val arity: Int?): LoxCallable {
    override fun arity(): Int? = arity
    override fun toString(): String = "<native fun $name:#${arity() ?: "vararg"}>"
}

// TODO: STATIC AND PROPERTY METHODS chapter 12.
class LoxClass(val name: String, val methods: Map<String, LoxFunction>): LoxCallable {
    override fun call(args: List<Any?>): Any {
        val obj = LoxObject(this, mutableMapOf(), methods.toMutableMap())
        methods["init"]?.bind(obj)?.call(args)
        return obj
    }

    override fun arity(): Int = methods["init"]?.arity() ?: 0

    override fun bind(obj: LoxObject): LoxCallable = this

    override fun toString(): String = "<class $name>"
}

class LoxObject(val klass: LoxClass, private val props: MutableMap<String, Any?>, private val methods: MutableMap<String, LoxFunction>) {
    override fun toString(): String = "<obj $klass>"

    fun getProp(name: Token): Any? {
        if (props.containsKey(name.lexeme)) {
            return props[name.lexeme]
        }

        if (methods.containsKey(name.lexeme)) {
            return  methods[name.lexeme]!!.bind(this)
        }

        throw InterpreterError(name, "Undefined property '${name.lexeme}' on object")
    }

    fun setProp(name: Token, value: Any?) {
        props[name.lexeme] = value
    }
}