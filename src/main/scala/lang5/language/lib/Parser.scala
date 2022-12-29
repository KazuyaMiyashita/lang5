package lang5.language.lib

import scala.collection.immutable.{AbstractSeq, LinearSeq}

trait Parser[+A] { self =>

  import Parser.*

  def parse(in: String): ParseResult[A]

  /** このパーサーによって終端することを要請する。すなわち消費できなかった文字列があると失敗とする
    * QUESTION: この実装は場当たり的ではないか？
    */
  final def parseAll: Parser[A] = { (in: String) =>
    self.parse(in).flatMap { s =>
      Either.cond(
        s.remaining.isEmpty,
        s, {
          val remaining =
            if (s.remaining.length <= 20) { s.remaining }
            else { s.remaining.substring(0, 20) + " ..." }
          ParseFailure(s"Parsing should have been terminated, but the following strings remained: $remaining")
        }
      )
    }
  }

  final def map[A2](f: A => A2): Parser[A2] = { (in: String) =>
    self.parse(in).map { case ParseSucceed(result, remaining) =>
      ParseSucceed(f(result), remaining)
    }
  }

}

object Parser {

  type ParseResult[+A] = Either[ParseFailure, ParseSucceed[A]]
  final case class ParseFailure(message: String)
  final case class ParseSucceed[+A](result: A, remaining: String)

  /** atomic parsing expression
    * 入力文字列の先頭と一致した場合に成功し、その文字列を消費する。一致しなかった場合失敗する。
    */
  def atomic(value: String): Parser[String] = { (in: String) =>
    Either.cond(
      in.startsWith(value),
      ParseSucceed(value, in.substring(value.length)),
      ParseFailure(s"Expected $value, but does not exist.")
    )
  }

  /** 並び
    * PEGの表記で e1 e2 と表されるもの
    * 並び e1 e2 は、まず e1 を呼び出し、 e1 が成功なら続いて e1 が消費した部分を除いて e2 を呼び出し、
    * その結果を全体の結果として返す。
    * e1 か e2 のいずれかが失敗した場合、並び e1 e2 全体が失敗する。
    */
  final def sequence[A1, A2](e1: => Parser[A1], e2: => Parser[A2]): Parser[(A1, A2)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
    } yield ParseSucceed((r1.result, r2.result), r2.remaining)
  }

  /** 並び e1 e2 の間にスペース等の区切り文字 delimiter が存在する場合、それを読み飛ばすためのユーティリティ */
  final def sequenceWithout[A1, A2](
      delimiter: => Parser[_]
  )(e1: => Parser[A1], e2: => Parser[A2]): Parser[(A1, A2)] = {
    sequence(dropRight(e1, delimiter), e2)
  }

  final def sequence[A1, A2, A3](e1: => Parser[A1], e2: => Parser[A2], e3: => Parser[A3]): Parser[(A1, A2, A3)] = {
    (in: String) =>
      for {
        r1 <- e1.parse(in)
        r2 <- e2.parse(r1.remaining)
        r3 <- e3.parse(r2.remaining)
      } yield ParseSucceed((r1.result, r2.result, r3.result), r3.remaining)
  }

  final def sequenceWithout[A1, A2, A3](
      delimiter: => Parser[_]
  )(e1: => Parser[A1], e2: => Parser[A2], e3: => Parser[A3]): Parser[(A1, A2, A3)] = {
    sequence(dropRight(e1, delimiter), dropRight(e2, delimiter), e3)
  }

  final def sequence[A1, A2, A3, A4](
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4]
  ): Parser[(A1, A2, A3, A4)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
      r3 <- e3.parse(r2.remaining)
      r4 <- e4.parse(r3.remaining)
    } yield ParseSucceed((r1.result, r2.result, r3.result, r4.result), r4.remaining)
  }

  final def sequenceWithout[A1, A2, A3, A4](
      delimiter: => Parser[_]
  )(
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4]
  ): Parser[(A1, A2, A3, A4)] = {
    sequence(dropRight(e1, delimiter), dropRight(e2, delimiter), dropRight(e3, delimiter), e4)
  }

  final def sequence[A1, A2, A3, A4, A5](
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4],
      e5: => Parser[A5]
  ): Parser[(A1, A2, A3, A4, A5)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
      r3 <- e3.parse(r2.remaining)
      r4 <- e4.parse(r3.remaining)
      r5 <- e5.parse(r4.remaining)
    } yield ParseSucceed(
      (
        r1.result,
        r2.result,
        r3.result,
        r4.result,
        r5.result
      ),
      r5.remaining
    )
  }

  final def sequenceWithout[A1, A2, A3, A4, A5](
      delimiter: => Parser[_]
  )(
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4],
      e5: => Parser[A5]
  ): Parser[(A1, A2, A3, A4, A5)] = {
    sequence(dropRight(e1, delimiter), dropRight(e2, delimiter), dropRight(e3, delimiter), dropRight(e4, delimiter), e5)
  }

  final def sequence[A1, A2, A3, A4, A5, A6](
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4],
      e5: => Parser[A5],
      e6: => Parser[A6]
  ): Parser[(A1, A2, A3, A4, A5, A6)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
      r3 <- e3.parse(r2.remaining)
      r4 <- e4.parse(r3.remaining)
      r5 <- e5.parse(r4.remaining)
      r6 <- e6.parse(r5.remaining)
    } yield ParseSucceed(
      (
        r1.result,
        r2.result,
        r3.result,
        r4.result,
        r5.result,
        r6.result
      ),
      r6.remaining
    )
  }

  final def sequenceWithout[A1, A2, A3, A4, A5, A6](delimiter: => Parser[_])(
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4],
      e5: => Parser[A5],
      e6: => Parser[A6]
  ): Parser[(A1, A2, A3, A4, A5, A6)] = {
    sequence(
      dropRight(e1, delimiter),
      dropRight(e2, delimiter),
      dropRight(e3, delimiter),
      dropRight(e4, delimiter),
      dropRight(e5, delimiter),
      e6
    )
  }

  final def sequence[A1, A2, A3, A4, A5, A6, A7](
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4],
      e5: => Parser[A5],
      e6: => Parser[A6],
      e7: => Parser[A7]
  ): Parser[(A1, A2, A3, A4, A5, A6, A7)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
      r3 <- e3.parse(r2.remaining)
      r4 <- e4.parse(r3.remaining)
      r5 <- e5.parse(r4.remaining)
      r6 <- e6.parse(r5.remaining)
      r7 <- e7.parse(r6.remaining)
    } yield ParseSucceed(
      (
        r1.result,
        r2.result,
        r3.result,
        r4.result,
        r5.result,
        r6.result,
        r7.result
      ),
      r7.remaining
    )
  }

  final def sequence[A1, A2, A3, A4, A5, A6, A7, A8](
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4],
      e5: => Parser[A5],
      e6: => Parser[A6],
      e7: => Parser[A7],
      e8: => Parser[A8]
  ): Parser[(A1, A2, A3, A4, A5, A6, A7, A8)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
      r3 <- e3.parse(r2.remaining)
      r4 <- e4.parse(r3.remaining)
      r5 <- e5.parse(r4.remaining)
      r6 <- e6.parse(r5.remaining)
      r7 <- e7.parse(r6.remaining)
      r8 <- e8.parse(r7.remaining)
    } yield ParseSucceed(
      (
        r1.result,
        r2.result,
        r3.result,
        r4.result,
        r5.result,
        r6.result,
        r7.result,
        r8.result
      ),
      r8.remaining
    )
  }

  final def sequenceWithout[A1, A2, A3, A4, A5, A6, A7, A8](delimiter: => Parser[_])(
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4],
      e5: => Parser[A5],
      e6: => Parser[A6],
      e7: => Parser[A7],
      e8: => Parser[A8]
  ): Parser[(A1, A2, A3, A4, A5, A6, A7, A8)] = {
    sequence(
      dropRight(e1, delimiter),
      dropRight(e2, delimiter),
      dropRight(e3, delimiter),
      dropRight(e4, delimiter),
      dropRight(e5, delimiter),
      dropRight(e6, delimiter),
      dropRight(e7, delimiter),
      e8
    )
  }

  final def sequence[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4],
      e5: => Parser[A5],
      e6: => Parser[A6],
      e7: => Parser[A7],
      e8: => Parser[A8],
      e9: => Parser[A9],
      e10: => Parser[A10],
      e11: => Parser[A11]
  ): Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
      r3 <- e3.parse(r2.remaining)
      r4 <- e4.parse(r3.remaining)
      r5 <- e5.parse(r4.remaining)
      r6 <- e6.parse(r5.remaining)
      r7 <- e7.parse(r6.remaining)
      r8 <- e8.parse(r7.remaining)
      r9 <- e9.parse(r8.remaining)
      r10 <- e10.parse(r9.remaining)
      r11 <- e11.parse(r10.remaining)
    } yield ParseSucceed(
      (
        r1.result,
        r2.result,
        r3.result,
        r4.result,
        r5.result,
        r6.result,
        r7.result,
        r8.result,
        r9.result,
        r10.result,
        r11.result
      ),
      r11.remaining
    )
  }

  /** 選択
    * PEGの表記で e1 / e2 と表されるもの
    * 選択 e1 / e2 は、まず e1 を呼び出し、 e1 が成功ならそれを結果として即座に返す。
    * あるいは e1 が失敗なら入力を e1 を呼び出す前の位置にバックトラッキングして e2 を呼び出し、 e2 の結果を返す。
    */
  final def orderedChoice[A](e1: => Parser[A], e2: => Parser[A]): Parser[A] = { (in: String) =>
    e1.parse(in).orElse(e2.parse(in))
  }

  /** 3つ以上の選択のためのユーティリティ
    * e1 / e2 / ...
    */
  final def orderedChoiceMulti[A](e1: => Parser[A], es: => Parser[A]*): Parser[A] = { (in: String) =>
    es.toList match {
      case e2 :: tail => orderedChoiceMulti(orderedChoice(e1, e2), tail: _*).parse(in)
      case Nil        => e1.parse(in)
    }
  }

  /** 1個以上
    * PEGの表記で e+ と表されるもの
    * PEGにおける繰り返しは常に貪欲でありマッチし続ける限り入力を消費するが、それだけではなく、正規表現とは異なりバックトラックしない。
    */
  final def oneOrMore[A](e: => Parser[A]): Parser[(A, List[A])] = {
    sequence(e, zeroOrMore(e))
  }

  final def oneOrMoreWithout[A](delimiter: => Parser[_])(e: => Parser[A]): Parser[(A, List[A])] = {
    sequence(dropRight(e, delimiter), zeroOrMoreWithout(delimiter)(e))
  }

  /** 0個以上
    * PEGの表記で e* と表されるもの
    * PEGにおける繰り返しは常に貪欲でありマッチし続ける限り入力を消費するが、それだけではなく、正規表現とは異なりバックトラックしない。
    */
  final def zeroOrMore[A](e: => Parser[A]): Parser[List[A]] = { (in: String) =>
    e.parse(in) match {
      case Left(_) => Right(ParseSucceed(Nil, in)) // パース出来なかった場合、空の結果を成功として返し、入力を消費しない。
      case Right(s1) =>
        val s2: ParseSucceed[List[A]] = zeroOrMore(e).parse(s1.remaining) match {
          case Left(_)      => throw new Exception("unreachable")
          case Right(value) => value
        }
        Right(ParseSucceed(s1.result :: s2.result, s2.remaining))
    }
  }

  final def zeroOrMoreWithout[A](delimiter: => Parser[_])(e: => Parser[A]): Parser[List[A]] = {
    zeroOrMore(dropRight(e, delimiter))
  }

  /** 省略可能
    * PEGの表記で e? と表されるもの
    * e によるパースに成功した場合はその結果をSomeに包んで返し、失敗した場合は None を返す。失敗したときは文字列は消費しない。
    */
  final def optional[A](e: => Parser[A]): Parser[Option[A]] = { (in: String) =>
    e.parse(in) match {
      case Left(_)  => Right(ParseSucceed(None, in))
      case Right(s) => Right(ParseSucceed(Some(s.result), s.remaining))
    }
  }

  /** 右側を読み飛ばすためのユーティリティ
    * parser で指定された文字列をパースし、その直後に next で指定された文字列がある場合、
    * next でパースした部分まで文字列を消費し、 parser の結果を返す
    */
  def dropRight[A](parser: Parser[A], next: => Parser[_]): Parser[A] = {
    sequence(parser, optional(next)).map((a, _) => a)
  }

}
