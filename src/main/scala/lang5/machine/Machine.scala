package lang5.machine

class Machine {

  import Machine.*

  // memory

  private val memory: Array[Byte] = new Array(MemoryLimit)

  private var programCounter = 0
  private var functionPtr = 0 // initialized in load()
  private var stackPtr = 0 // initialized in run()

  // public methods

  def load(program: Array[Byte]): Unit = {
    System.arraycopy(program, 0, memory, 0, program.length)
    functionPtr = program.length
  }

  def run(argc: Byte, argv: Array[Byte]): Unit = {
    import Operations.*

    memory(functionPtr) = 0 // 関数同様、先頭はリターンアドレスを用意するが、これは使わない
    memory(functionPtr + 1) = 0 // 関数同様、次はリターン関数ポインタを用意するが、これは使わない
    memory(functionPtr + 2) = argc // 関数同様、その次は引数の個数
    System.arraycopy(argv, 0, memory, functionPtr + 2, argc)
    stackPtr = functionPtr + 3 + argc // 関数同様、引数の後にスタックポインタが設定される。ローカル変数はプログラムの先頭で指定される。

    while (true) {
      val op = read()
      op match {
        case Nop  => ()
        case Push => push(read())
        case Pop  => pop()
        case Jump => programCounter = read()
//        case FunPtr    => functionPtr = read()
//        case StkPtr    => stackPtr = functionPtr + read()
        case ReadArg   => push(memory(functionPtr + 2 + read()))
        case SetReturn => memory(functionPtr + memory(functionPtr + 1) + memory(functionPtr + 2) + read()) = pop()
        case Return =>
          memory(1) = memory(0)
          memory(0) = memory(functionPtr + memory(functionPtr + 1) + memory(functionPtr + 2))
          stackPtr = 1
          functionPtr = 0
          programCounter = pop()
        case Add => push((pop().toInt + pop().toInt).toByte)
        case End => return
      }
    }
  }

  def result(length: Int): Array[Byte] = {
    val array = new Array[Byte](length)
    System.arraycopy(memory, stackPtr, array, 0, length)
    array
  }

  // private methods

  private def read(): Byte = {
    val p = memory(programCounter)
    programCounter += 1
    p
  }

  private def push(v: Byte): Unit = { memory(stackPtr) = v; stackPtr += 1 }
  private def pop(): Byte = { stackPtr -= 1; memory(stackPtr) }

}

object Machine {

  private val MemoryLimit: Int = 16 * 1024

}
