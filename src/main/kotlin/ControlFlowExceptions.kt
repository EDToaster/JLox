open class ControlFlowException: RuntimeException()

class BreakException: ControlFlowException()
class ReturnException(val value: Any?): ControlFlowException()

class AssertionException: ControlFlowException()
