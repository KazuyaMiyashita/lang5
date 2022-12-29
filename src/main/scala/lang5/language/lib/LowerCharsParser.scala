package lang5.language.lib

import Parser.*

/** 空でないアルファベット小文字からなる列をパースする */
object LowerCharsParser extends Parser[String] {

  override def parse(in: String): ParseResult[String] = {
    oneOrMore(
      orderedChoiceMulti(
        atomic("a"),
        atomic("b"),
        atomic("c"),
        atomic("d"),
        atomic("e"),
        atomic("f"),
        atomic("g"),
        atomic("h"),
        atomic("i"),
        atomic("j"),
        atomic("k"),
        atomic("l"),
        atomic("m"),
        atomic("n"),
        atomic("o"),
        atomic("p"),
        atomic("q"),
        atomic("r"),
        atomic("s"),
        atomic("t"),
        atomic("u"),
        atomic("v"),
        atomic("w"),
        atomic("x"),
        atomic("y"),
        atomic("z")
      )
    ).map { (head, tail) => head :: tail }.map(_.mkString).parse(in)
  }

}
