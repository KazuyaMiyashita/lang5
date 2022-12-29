package lang5.language

object Ast {

  final case class Program(definitions: List[TopLevel])

  sealed trait TopLevel

  final case class GlobalVariableDefinition(name: String, expression: Expression) extends TopLevel

  final case class FunctionDefinition(name: String, args: List[String], body: Expression) extends TopLevel

  sealed trait Expression

  final case class BinaryExpression(operator: Operator, lhs: Expression, rhs: Expression) extends Expression

  final case class IntegerLiteral(value: Int) extends Expression

  /** 変数の参照を表す */
  final case class Identifier(name: String) extends Expression

  /** 変数の代入を表す */
  final case class Assignment(name: String, expression: Expression) extends Expression

  final case class BlockExpression(elements: List[Expression]) extends Expression

  final case class WhileExpression(condition: Expression, body: Expression) extends Expression

  final case class IfExpression(condition: Expression, thenClause: Expression, elseClause: Option[Expression])
    extends Expression

  final case class FunctionCall(name: String, args: List[Expression]) extends Expression

  def add(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Add, lhs, rhs)
  }

  def subtract(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Subtract, lhs, rhs)
  }

  def multiply(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Multiply, lhs, rhs)
  }

  def divide(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Divide, lhs, rhs)
  }

  def lessThan(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.LessThan, lhs, rhs)
  }

  def lessOrEqual(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.LessOrEqual, lhs, rhs)
  }

  def greaterThan(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.GreaterThan, lhs, rhs)
  }

  def greaterOrEqual(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.GreaterOrEqual, lhs, rhs)
  }

  def equal(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Equal, lhs, rhs)
  }

  def notEqual(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.NotEqual, lhs, rhs)
  }

  def integer(value: Int): IntegerLiteral = {
    IntegerLiteral(value)
  }

  def identifier(name: String): Identifier = Identifier(name)

  def assignment(name: String, expression: Expression): Assignment = Assignment(name, expression)

  def block(elements: Expression*): BlockExpression = BlockExpression(elements.toList)

  def _while(condition: Expression, body: Expression) = WhileExpression(condition, body)

  def _if(condition: Expression, thenClause: Expression, elseClause: Expression): IfExpression =
    IfExpression(condition, thenClause, Some(elseClause))

  def _if(condition: Expression, thenClause: Expression): IfExpression =
    IfExpression(condition, thenClause, None)

  def defFun(name: String, args: List[String], body: Expression): FunctionDefinition =
    FunctionDefinition(name, args, body)

  def callFun(name: String, args: Expression*): FunctionCall = FunctionCall(name, args.toList)

  def makeProgram(definitions: TopLevel*): Program = Program(definitions.toList)

  def globalVarDef(name: String, expression: Expression): GlobalVariableDefinition =
    GlobalVariableDefinition(name, expression)
  
}
