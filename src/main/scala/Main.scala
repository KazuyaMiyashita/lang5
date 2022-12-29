import lang5.machine.{EasyMachine2, IntermediatePresentation}
import lang5.machine.IntermediatePresentation.*

object Main extends App {
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
  println("*** human readable byte codes ***")
  println(ip.prettyString)
  val machine = new EasyMachine2
  val program = ip.compile
  machine.load(program)
  println("*** running... (argument is empty) ***")
  machine.run(Array.emptyByteArray)
  val result = machine.result(4)
  println(s"*** run result ***")
  println(s"${result.mkString("[", ",", "]")}")
}
