import lang5.language.{AstPrinter, LanguageCompiler, LanguageParser}
import lang5.machine.{EasyMachine2, IntermediatePresentation}
import lang5.machine.IntermediatePresentation.*

object Main extends App {
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

  println("*** source file ***")
  println(source)

  val ast = LanguageParser.parse(source).fold(err => throw new Exception(err.message), _.result)
  println("*** abstract syntax tree ***")
  println(AstPrinter.astToString(ast))
  println()

  val ip = LanguageCompiler.compile(ast)
  println("*** human readable byte codes ***")
  println(ip.prettyString)

  val machine = new EasyMachine2
  val program = ip.compile
  machine.load(program)
  println("*** running... (args = []) ***")
  println()
  machine.run(Array.emptyByteArray)
  val result = java.nio.ByteBuffer.allocate(4).put(machine.result(4)).getInt(0)

  println(s"*** run result ***")
  println(result)
  println("succeed.")
}
