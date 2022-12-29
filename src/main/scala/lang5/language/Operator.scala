package lang5.language

sealed abstract class Operator(val name: String)
object Operator {
  case object Add extends Operator("+")
  case object Subtract extends Operator("-")
  case object Multiply extends Operator("*")
  case object Divide extends Operator("/")

  case object LessThan extends Operator("<")
  case object LessOrEqual extends Operator("<=")
  case object GreaterThan extends Operator(">")
  case object GreaterOrEqual extends Operator(">=")
  case object Equal extends Operator("==")
  case object NotEqual extends Operator("!=")

}
