class BreakException: RuntimeException()
class ReturnException(val value: Any?): RuntimeException()