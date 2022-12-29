import lang5.language.{LanguageCompiler, LanguageParser}
import lang5.machine.{Machine, MachineLanguage}
import lang5.machine.Operations.*

object Main extends App {
  val machine = new Machine
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
  machine.run(0, new Array[Byte](0))
  val result = machine.result(1)
  println(result.mkString)
}
