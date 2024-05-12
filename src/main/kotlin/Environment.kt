class VariableMap : HashMap<String, Any?>() {

}

class Environment(val enclosing: Environment? = null) {
    private val bindings: MutableMap<String, Any?> = mutableMapOf()

    fun declare(key: String, value: Any?) {
        bindings[key] = value
    }

    fun assign(key: String, value: Any?) {
        if (bindings.containsKey(key)) {
            bindings[key] = value
        } else {
            enclosing?.assign(key, value) ?: throw RuntimeException("Undeclared variable $key")
        }
    }

    operator fun get(key: String): Any? {
        return if (bindings.containsKey(key)) {
            bindings[key]
        } else {
            enclosing?.get(key) ?: throw RuntimeException("Undeclared variable $key")
        }
    }
}