package lang5.language

import lang5.language.lib.Parser.ParseSucceed
import lang5.language.Ast.*
import org.scalatest.funsuite.AnyFunSuite

class LanguageParserTest extends AnyFunSuite {

  test("parse") {
    val source =
      """function main() {
        |    a = 1;
        |    b = 42;
        |    c = add(a, b);
        |    c
        |}
        |
        |function add(a, b) {
        |    a + b
        |}
        |""".stripMargin

    val result = LanguageParser.parse(source)
    val expect = Right(
      ParseSucceed(
        program(
          defFun(
            "main",
            List(),
            List(
              assignment("a", integer(1)),
              assignment("b", integer(42)),
              assignment(
                "c",
                callFun("add", List(identifier("a"), identifier("b")))
              ),
              identifier("c")
            )
          ),
          defFun(
            "add",
            List("a", "b"),
            List(
              add(identifier("a"), identifier("b"))
            )
          )
        ),
        remaining = ""
      )
    )

    assert(result == expect)
  }

}
