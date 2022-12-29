package lang5.language

import lang5.language.Ast.*
import lang5.language.lib.{IntegerLiteralParser, LowerCharsParser, Parser}
import lang5.language.lib.Parser.*

import scala.annotation.tailrec

object LanguageParser extends Parser[Program] {

  def programParser: Parser[Program] =
    zeroOrMoreWithout(spaces)(topLevelDefinitionParser).map(topLevels => Program(topLevels))

  def topLevelDefinitionParser: Parser[TopLevel] =
    orderedChoice(globalVariableDefinitionParser, functionDefinitionParser)

  def globalVariableDefinitionParser: Parser[GlobalVariableDefinition] =
    sequenceWithout(spaces)(
      atomic("global"),
      identifierStringParser,
      atomic("="),
      expressionParser
    ).map { (_, name, _, expression) =>
      GlobalVariableDefinition(name, expression)
    }

  def functionDefinitionParser: Parser[FunctionDefinition] = sequenceWithout(spaces)(
    atomic("define"),
    identifierStringParser,
    atomic("("),
    optional(
      sequenceWithout(spaces)(
        identifierStringParser,
        zeroOrMore(sequence(atomic(","), identifierStringParser))
      )
    ),
    atomic(")"),
    blockExpressionParser
  ).map { (_, name, _, argsOpt, _, block) =>
    val args = argsOpt match {
      case None    => Nil
      case Some(a) => a._1 :: a._2.map(_._2)
    }
    FunctionDefinition(name, args, block)
  }

  def lineParser = orderedChoiceMulti(
    whileExpressionParser,
    ifExpressionParser,
    assignmentParser,
    expressionLineParser,
    blockExpressionParser
  )

  def ifExpressionParser: Parser[IfExpression] = sequenceWithout(spaces)(
    atomic("if"),
    atomic("("),
    expressionParser,
    atomic(")"),
    lineParser,
    optional(
      sequenceWithout(spaces)(
        atomic("else"),
        lineParser
      )
    )
  ).map { (_, _, condition, _, thenClause, _elseClause) =>
    IfExpression(condition, thenClause, _elseClause.map(_._2))
  }

  def whileExpressionParser: Parser[WhileExpression] = sequenceWithout(spaces)(
    atomic("while"),
    atomic("("),
    expressionParser,
    atomic(")"),
    lineParser
  ).map { (_, _, condition, _, body) =>
    WhileExpression(condition, body)
  }

  def blockExpressionParser: Parser[BlockExpression] = sequenceWithout(spaces)(
    atomic("{"),
    zeroOrMoreWithout(spaces)(expressionParser),
    atomic("}")
  ).map { case (_, elements, _) =>
    BlockExpression(elements)
  }

  def assignmentParser: Parser[Assignment] = sequenceWithout(spaces)(
    identifierStringParser,
    atomic("="),
    expressionParser,
    atomic(";")
  ).map { (name, _, expression, _) =>
    Assignment(name, expression)
  }

  def expressionLineParser: Parser[Expression] = sequenceWithout(spaces)(
    expressionParser,
    atomic(";")
  ).map(_._1)

  def expressionParser: Parser[Expression] = comparativeParser

  @tailrec
  def listToTree(head: Expression, tail: List[(Operator, Expression)]): Expression = {
    tail match {
      case (op, expr) :: next => listToTree(BinaryExpression(op, head, expr), next)
      case Nil                => head
    }
  }

  def comparativeParser: Parser[Expression] = {
    sequenceWithout(spaces)(
      additiveParser,
      zeroOrMoreWithout(spaces)(
        sequenceWithout(spaces)(
          orderedChoiceMulti(
            atomic("<").map(_ => Operator.LessThan),
            atomic(">").map(_ => Operator.GreaterThan),
            atomic("<=").map(_ => Operator.LessOrEqual),
            atomic(">=").map(_ => Operator.GreaterOrEqual),
            atomic("==").map(_ => Operator.Equal),
            atomic("!=").map(_ => Operator.NotEqual)
          ),
          additiveParser
        )
      )
    ).map { (head, tail) => listToTree(head, tail) }
  }

  def additiveParser: Parser[Expression] =
    sequenceWithout(spaces)(
      multitiveParser,
      zeroOrMoreWithout(spaces)(
        sequenceWithout(spaces)(
          orderedChoiceMulti(
            atomic("+").map(_ => Operator.Add),
            atomic("-").map(_ => Operator.Subtract)
          ),
          multitiveParser
        )
      )
    ).map { (head, tail) => listToTree(head, tail) }

  def multitiveParser: Parser[Expression] =
    sequenceWithout(spaces)(
      primaryParser,
      zeroOrMoreWithout(spaces)(
        sequenceWithout(spaces)(
          orderedChoiceMulti(
            atomic("*").map(_ => Operator.Multiply),
            atomic("/").map(_ => Operator.Divide)
          ),
          primaryParser
        )
      )
    ).map { (head, tail) => listToTree(head, tail) }

  def primaryParser: Parser[Expression] =
    orderedChoiceMulti(
      sequenceWithout(spaces)(atomic("("), expressionParser, atomic(")")).map { (_, expr, _) => expr },
      integerLiteralParser,
      functionCallParser,
      identifierParser
    )

  def integerLiteralParser: Parser[IntegerLiteral] = IntegerLiteralParser.map(IntegerLiteral.apply)

  def functionCallParser: Parser[FunctionCall] =
    sequenceWithout(spaces)(
      identifierStringParser,
      atomic("("),
      optional(
        sequenceWithout(spaces)(
          expressionParser,
          zeroOrMoreWithout(spaces)(
            sequenceWithout(spaces)(
              atomic(","),
              expressionParser
            )
          )
        )
      ),
      atomic(")")
    ).map { (name, _, argOpt, _) =>
      val args = argOpt match {
        case None    => Nil
        case Some(a) => a._1 :: a._2.map(_._2)
      }
      FunctionCall(name, args)
    }

  def identifierStringParser: Parser[String] = LowerCharsParser

  def identifierParser: Parser[Identifier] = identifierStringParser.map(Identifier.apply)

  // QUESTION: 改行もここに含めるのだろうか？
  def spaces: Parser[Unit] = oneOrMore(
    orderedChoiceMulti(
      atomic(" "),
      atomic("\n")
    )
  ).map(_ => ())

  override def parse(in: String): ParseResult[Program] = programParser.requireTerminal.parse(in)

}
