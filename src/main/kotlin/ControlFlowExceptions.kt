open class ControlFlowException: RuntimeException() {
    override fun fillInStackTrace(): Throwable {
        // Slight optimization to speed up control flow
        return this
    }
}

class BreakException: ControlFlowException()
class ReturnException(val value: Any?): ControlFlowException()
class ExitException(val retCode: Int): ControlFlowException()

class AssertionException: ControlFlowException()
