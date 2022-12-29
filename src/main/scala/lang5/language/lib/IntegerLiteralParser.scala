package lang5.language.lib

import lang5.language.lib.Parser.*

object IntegerLiteralParser extends Parser[Int] {

  sealed trait Result {
    def toInt: Int
  }

  object Result {
    case class Zero() extends Result {
      override def toInt: Int = 0
    }

    case class Negative(pos: Positive) extends Result {
      override def toInt: Int = -1 * pos.toInt
    }

    // 1桁ごとの数値がIntで入ってくる。 123なら head=1, tail=[2, 3]
    case class Positive(head: Int, tail: List[Int]) extends Result {
      override def toInt: Int = {
        (head :: tail).reverse.zipWithIndex.map { (v, i) =>
          v * Math.pow(10, i).toInt
        }.sum
      }
    }
  }

  /** integer <- zero / negative / positive
    * zero <- "0"
    * negative <- "-" positive
    * positive <- one_to_nine zero_to_nine*
    * one_to_nine <- "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
    * zero_to_nine <- zero / one_to_nine
    */
  override def parse(in: String): ParseResult[Int] = Integer.map(_.toInt).parse(in)

  lazy val Integer: Parser[Result] = orderedChoiceMulti(Zero, Negative, Positive)
  lazy val Zero: Parser[Result.Zero] = atomic("0").map(_ => Result.Zero())
  lazy val Negative: Parser[Result.Negative] =
    sequence(atomic("-"), Positive).map[Result.Negative]((_, pos) => Result.Negative(pos))
  lazy val Positive: Parser[Result.Positive] =
    sequence(OneToNine, zeroOrMore(ZeroToNine)).map((i, is) => Result.Positive(i, is))
  lazy val OneToNine: Parser[Int] = orderedChoiceMulti(
    atomic("1").map(_ => 1),
    atomic("2").map(_ => 2),
    atomic("3").map(_ => 3),
    atomic("4").map(_ => 4),
    atomic("5").map(_ => 5),
    atomic("6").map(_ => 6),
    atomic("7").map(_ => 7),
    atomic("8").map(_ => 8),
    atomic("9").map(_ => 9)
  )
  lazy val ZeroToNine: Parser[Int] = orderedChoice(atomic("0").map(_ => 0), OneToNine)

}
