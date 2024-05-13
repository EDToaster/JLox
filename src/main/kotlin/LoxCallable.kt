interface LoxCallable {
    fun call(args: List<Any?>): Any?
    // returns null for vararg
    fun arity(): Int?
}