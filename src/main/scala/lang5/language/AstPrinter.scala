package lang5.language

import lang5.language.Ast.*

object AstPrinter {

  def astToString(program: Ast.Program): String = {
    s"""program(
       |${program.definitions.map(df => _defFun(df)).mkString(",\n")}
       |)
       |""".stripMargin
  }

  extension (expression: Expression) {
    def lengthFromLeaf: Int = {
      expression match {
        case BinaryExpression(operator, lhs, rhs) => 1 + Math.min(lhs.lengthFromLeaf, rhs.lengthFromLeaf)
        case IntegerLiteral(value)                => 0
        case Identifier(name)                     => 0
        case Assignment(name, expr)               => 1 + expr.lengthFromLeaf
        case FunctionCall(name, args)             => 1 + args.map(_.lengthFromLeaf).minOption.getOrElse(0)
      }
    }
  }

  extension (str: String) {
    def toIndent(num: Int): String = str.split("\n").map(line => "  " * num + line).mkString("\n")
  }
  extension (strs: List[String]) {
    def nilOrMkString(sep: String): String = if (strs.isEmpty) "Nil" else strs.mkString(sep)
    def nilOrMkString(start: String, sep: String, end: String): String =
      if (strs.isEmpty) "Nil" else strs.mkString(start, sep, end)
  }

  private def _defFun(fd: FunctionDefinition): String =
    s"""defFun(
       |  name = "${fd.name}",
       |  args = ${fd.args.nilOrMkString("List(", ", ", ")")},
       |  body = ${fd.body.map(expr => expressionInline(expr, 0)).nilOrMkString("List(\n    ", ",\n    ", "\n  )")}
       |)
       |""".stripMargin.toIndent(1)

  private def expressionInline(expr: Expression, indent: Int): String = {
    (expr match {
      case BinaryExpression(operator, lhs, rhs) =>
        val op = operator match {
          case Operator.Add      => "add"
          case Operator.Multiply => "mul"
        }
        s"""$op(${expressionInline(lhs, 0)}, ${expressionInline(rhs, 0)})"""
      case IntegerLiteral(value)        => value.toString
      case Identifier(name)             => s"""identifier("$name")"""
      case Assignment(name, expression) => s"""assignment("$name", ${expressionInline(expression, 0)})"""
      case FunctionCall(name, args) =>
        s"""callFun("$name", ${args.map(arg => expressionInline(arg, 0)).nilOrMkString("List(", ", ", ")")}"""
    }).toIndent(indent)
  }

//  private def _expression(expr: Expression, indent: Int): String = {
//    if (expr.lengthFromLeaf <= 2) {
//      expr match {
//        case BinaryExpression(operator, lhs, rhs) =>
//          val op = operator match {
//            case Operator.Add      => "add"
//            case Operator.Multiply => "mul"
//          }
//          s"""$op(${_expression(lhs, 0)}, ${_expression(rhs, 0)})"""
//        case IntegerLiteral(value)        => ???
//        case Identifier(name)             => ???
//        case Assignment(name, expression) => ???
//        case FunctionCall(name, args)     => ???
//      }
//    } else {
//      (expr match {
//        case BinaryExpression(operator, lhs, rhs) =>
//          val op = operator match {
//            case Operator.Add      => "add"
//            case Operator.Multiply => "mul"
//          }
//          s"""$op(
//             |${_expression(lhs, indent + 1)},
//             |${_expression(rhs, indent + 1)},
//             |)
//             |""".stripMargin
//        case IntegerLiteral(value) => value.toString
//        case Identifier(name)      => s"identifier($name)"
//        case Assignment(name, expression) =>
//          expression match {
//            case _: IntegerLiteral   => s"""assignment("$name", ${_expression(expression, 0)})""".stripMargin
//            case _: Identifier       => ???
//            case _: Assignment       => ???
//            case _: BinaryExpression => ???
//            case _: FunctionCall     => ???
//          }
//        case FunctionCall(name, args) =>
//          s"""callFun(
//             |  $name,
//             |  args = ${args.map(arg => _expression(arg, indent)).nilOrMkString("List(", ",\n", ")")}
//             |)
//             |""".stripMargin
//      }).toIndent(indent)
//    }
//
//  }

}
