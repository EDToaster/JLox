import java.io.Reader

interface PeekingIterator<T> : Iterator<T> {
    fun peek(): T
}

fun <T> Iterator<T>.peeking(): PeekingIterator<T> =
    if (this is PeekingIterator)
        this
    else
        object : PeekingIterator<T> {
            private var cached = false

            @Suppress("UNCHECKED_CAST")
            private var element: T = null as T
                get() {
                    if (!cached)
                        field = this@peeking.next()
                    return field
                }

            override fun hasNext(): Boolean = cached || this@peeking.hasNext()

            override fun next(): T = element.also { cached = false }

            override fun peek(): T = element.also { cached = true }
        }

fun Reader.iter(): Iterator<Char> = iterator {
    while (true) {
        val r = read().also { if (it == -1) return@iterator }
        yield(r.toChar())
    }
}