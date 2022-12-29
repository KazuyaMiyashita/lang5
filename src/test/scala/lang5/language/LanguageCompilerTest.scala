package lang5.language

import lang5.machine.IntermediatePresentation as IP
import org.scalatest.funsuite.AnyFunSuite

class LanguageCompilerTest extends AnyFunSuite {

  test("ast to machine intermediate presentation") {
    val prog = {
      import lang5.language.Ast.*
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
      )
    }

    val result = LanguageCompiler.compile(prog)

    println(result)

    val ip = {
      import IP.*
      import Elem.*
      import Elem.Operation.*
      IP(
        List(
          FunctionDef(
            "main",
            List(
              Iconst_1,
              Istore_0,
              Bipush,
              ConstValue(42),
              Istore_1,
              Iload_0,
              Iload_1,
              Invokestatic,
              FunctionRef("add"),
              Istore_2,
              Iload_2,
              Ireturn
            )
          ),
          FunctionDef(
            "add",
            List(
              Iload_0,
              Iload_1,
              Iadd,
              Ireturn
            )
          )
        )
      )
    }

    assert(result == ip)
  }

}
