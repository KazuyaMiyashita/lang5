import lang5.language.{LanguageCompiler, LanguageParser}
import lang5.machine.EasyMachine2

object Main extends App {
  val machine = new EasyMachine2
//  val source =
//    """define main() {
//      |  1
//      |}
//      |""".stripMargin
//  val ast = LanguageParser.requireTerminal
//    .parse(source)
//    .fold(err => throw new Exception(err.message), _.result)
//  val program = LanguageCompiler.compile(ast)
  val program = ???
  machine.load(program)
  machine.run(Array.emptyByteArray)
  val result = machine.result(1)
  println(result.mkString)
}
