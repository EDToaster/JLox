class Environment(val enclosing: Environment? = null) {
    private val bindings: MutableMap<String, Any?> = mutableMapOf()

    fun declare(key: String, value: Any?) {
        bindings[key] = value
    }

    // if skips < 0 then it is a global, propagate all the way to the top scope.
    fun assign(key: String, value: Any?, skips: Int) {
        if ((skips < 0 && enclosing != null) || skips > 0) {
            (enclosing ?: throw RuntimeException("Unexpected number of skips $skips")).assign(key, value, skips - 1)
        }

        bindings[key] = value
    }

    fun get(key: String, skips: Int): Any? {
        if ((skips < 0 && enclosing != null) || skips > 0) {
            return (enclosing ?: throw RuntimeException("Unexpected number of skips $skips")).get(key, skips - 1)
        }

        return bindings[key]
    }

    fun boundNames(): Set<String> = bindings.keys
}