package lang5.machine

import EasyMachine2.*
import IntermediatePresentation.*
import org.scalatest.funsuite.AnyFunSuite

class EasyMachine2Test extends AnyFunSuite {

  val ip = {
    import Elem.*
    import Operation.*
    IntermediatePresentation(
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

  test("intermediate presentation to pretty string") {
    val expected =
      """function main():
        |  0: iconst_1
        |  1: istore_0
        |  2: bipush  42
        |  4: istore_1
        |  5: iload_0
        |  6: iload_1
        |  7: invokestatic  [program address of add()]
        |  10: istore_2
        |  11: iload_2
        |  12: ireturn
        |
        |function add():
        |  0: iload_0
        |  1: iload_1
        |  2: iadd
        |  3: ireturn
        |""".stripMargin

    assert(ip.prettyString == expected)
  }

  test("intermediate presentation to compiled code") {
    val expected = {
      import Operations.*
      List[Byte](
        iconst_1,
        istore_0,
        bipush,
        42,
        istore_1,
        iload_0,
        iload_1,
        invokestatic,
        0,
        13,
        istore_2,
        iload_2,
        ireturn,
        iload_0,
        iload_1,
        iadd,
        ireturn
      )

    }

    assert(ip.compile.toList == expected)

  }

  test("run") {
    val program = ip.compile
    val machine = new EasyMachine2
    machine.load(program)
    machine.run(Array.emptyByteArray)
    val result = machine.result(4)
    val resultInt = java.nio.ByteBuffer.allocate(4).put(result).getInt(0)
    assert(resultInt == 43)
  }

}
