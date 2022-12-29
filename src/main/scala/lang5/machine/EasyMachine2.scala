package lang5.machine

class EasyMachine2 {

  import EasyMachine2.*

  private val programMemory: Array[Byte] = new Array(1024)
  private val memory: Array[Byte] = new Array(1024)

  private var programAddress = 0
  private var framePointer = 0
  private var stackPointer = 0

  private val temp4byteBuffer: java.nio.ByteBuffer = java.nio.ByteBuffer.allocate(4)

  def load(program: Array[Byte]): Unit = {
    System.arraycopy(program, 0, programMemory, 0, program.length)
  }

  private def read(): Byte = {
    val b = programMemory(programAddress)
    programAddress += 1
    b
  }

  private def push(b: Byte): Unit = {
    memory(stackPointer) = b
    stackPointer += 1
  }

  private def pop(): Byte = {
    stackPointer -= 1
    memory(stackPointer)
  }

  private def _pushInt(i: Int): Unit = {
    temp4byteBuffer.putInt(i)
    temp4byteBuffer.position(0).get(memory, stackPointer, 4)
    temp4byteBuffer.clear()
    stackPointer += 4
  }

  private def _popInt(): Int = {
    stackPointer -= 4
    val i = temp4byteBuffer.put(memory, stackPointer, 4).getInt(0)
    temp4byteBuffer.clear()
    i
  }

  // フレームの先頭16byteは4byteごとのローカル変数兼メソッドの引数の保管場所になっている

  private def _iload(n: Int): Unit = {
    System.arraycopy(memory, framePointer + 4 * n, memory, stackPointer, 4)
    stackPointer += 4
  }

  private def _istore(n: Int): Unit = {
    stackPointer -= 4
    System.arraycopy(memory, stackPointer, memory, framePointer + 4 * n, 4)
  }

  // フレームの16byte, 17byteはメソッドから抜ける時のプログラムアドレスの戻る位置が保管されている

  private def _getReturnProgramAddress(): Int = {
    temp4byteBuffer.put(0.asInstanceOf[Byte])
    temp4byteBuffer.put(0.asInstanceOf[Byte])
    temp4byteBuffer.put(memory, framePointer + 16, 2)
    val addr = temp4byteBuffer.getInt(0)
    temp4byteBuffer.clear()
    addr
  }

  private def _setReturnProgramAddress(addr: Int): Unit = {
    memory(framePointer + 16) = (addr >>> 8).asInstanceOf[Byte]
    memory(framePointer + 17) = addr.asInstanceOf[Byte]
  }

  // フレームの18byte, 19byteはメソッドから抜ける時のフレームポインタの戻る位置が保管されている

  private def _getReturnFramePointer(): Int = {
    temp4byteBuffer.put(0.asInstanceOf[Byte])
    temp4byteBuffer.put(0.asInstanceOf[Byte])
    temp4byteBuffer.put(memory, framePointer + 18, 2)
    val addr = temp4byteBuffer.getInt(0)
    temp4byteBuffer.clear()
    addr
  }

  private def _setReturnFramePointer(addr: Int): Unit = {
    memory(framePointer + 18) = (addr >>> 8).asInstanceOf[Byte]
    memory(framePointer + 19) = addr.asInstanceOf[Byte]
  }

  private def _setNewFramePointer(): Unit = {
    framePointer += 20
  }

  private def _setNewStackPointer(): Unit = {
    stackPointer = framePointer + 20
  }

  private def _bipush(b: Byte): Unit = {
    memory(stackPointer + 3) = b
    stackPointer += 4
  }

  private def _iadd(): Unit = {
    _pushInt(_popInt() + _popInt())
  }

  private def _ireturn(): Unit = {
    val returnProgramAddr = _getReturnProgramAddress()
    val returnFramePointer = _getReturnFramePointer()
    System.arraycopy(memory, framePointer + 20, memory, framePointer, 4)
    stackPointer = framePointer
    programAddress = returnProgramAddr
    framePointer = returnFramePointer
  }

  private def _invokestatic(jumpAddrMsb: Byte, jumpAddrLsb: Byte): Unit = {
    temp4byteBuffer.put(0.asInstanceOf[Byte])
    temp4byteBuffer.put(0.asInstanceOf[Byte])
    temp4byteBuffer.put(jumpAddrMsb)
    temp4byteBuffer.put(jumpAddrLsb)
    val jumpAddr = temp4byteBuffer.getInt(0)
    temp4byteBuffer.clear()
    val returnProgramAddr = programAddress
    val returnFramePtr = framePointer
    _setNewFramePointer()
    _setReturnProgramAddress(returnProgramAddr)
    _setReturnFramePointer(returnFramePtr)
    _setNewStackPointer()
    programAddress = jumpAddr
  }

  // argvは16バイトまで
  def run(argv: Array[Byte]): Unit = {
    import Operations.*

    if (argv.length > 16) {
      throw new Exception("The upper limit of argv is 16 bytes")
    }
    framePointer = 0
    System.arraycopy(argv, 0, memory, 0, argv.length)
    _setReturnProgramAddress(0)
    _setReturnFramePointer(0)
    _setNewStackPointer()

    val b = new scala.util.control.Breaks
    b.breakable {
      while (true) {
        read() match {
          case `nop`          => ()
          case `iconst_1`     => _bipush(1)
          case `iload_0`      => _iload(0)
          case `iload_1`      => _iload(1)
          case `iload_2`      => _iload(2)
          case `istore_0`     => _istore(0)
          case `istore_1`     => _istore(1)
          case `istore_2`     => _istore(2)
          case `bipush`       => _bipush(read())
          case `iadd`         => _iadd()
          case `ireturn`      => _ireturn(); if (programAddress == 0) b.break()
          case `invokestatic` => _invokestatic(read(), read())
        }
      }
    }

  }

  def result(loadBytes: Int): Array[Byte] = {
    val dest = new Array[Byte](loadBytes)
    System.arraycopy(memory, 0, dest, 0, loadBytes)
    dest
  }

}

object EasyMachine2 {

  object Operations {
    val nop: Byte = 0x00
    val iconst_1: Byte = 0x04
    val iload_0: Byte = 0x1a
    val iload_1: Byte = 0x1b
    val iload_2: Byte = 0x1c
    val istore_0: Byte = 0x3b
    val istore_1: Byte = 0x3c
    val istore_2: Byte = 0x3d
    val bipush: Byte = 0x10
    val iadd: Byte = 0x60
    val ireturn: Byte = 0xac.asInstanceOf[Byte]
    val invokestatic: Byte = 0xb8.asInstanceOf[Byte]
  }

  // intermediate presentation

  import IntermediatePresentation.*
  case class IntermediatePresentation(functionDefs: List[FunctionDef]) {
    def prettyString: String = {
      functionDefs
        .map { case FunctionDef(name, elems) =>
          val elemsWithIndex: List[(Elem, Int)] = elems zip accum(elems.map(_.bytes))
          val elemsString = elemsWithIndex.map { case (elem, index) =>
            elem match {
              case _: Elem.Operation   => s"\n  $index: ${elem.prettyName}"
              case Elem.ConstValue(_)  => s"  ${elem.prettyName}"
              case Elem.FunctionRef(_) => s"  ${elem.prettyName}"
            }
          }

          s"function $name():${elemsString.mkString}"
        }
        .mkString("", "\n\n", "\n")
    }

    def compile: Array[Byte] = {
      val functionIndex: Map[String, Int] =
        (functionDefs.map(_.name) zip accum(functionDefs.map(_.elems.map(_.bytes).sum))).toMap

      functionDefs.flatMap { case FunctionDef(_, elems) =>
        elems.flatMap {
          case Elem.FunctionRef(ref) =>
            val addrInt: Int = functionIndex(ref)
            (addrInt >>> 8).asInstanceOf[Byte] :: addrInt.asInstanceOf[Byte] :: Nil
          case op: Elem.Operation    => op.code :: Nil
          case Elem.ConstValue(code) => code :: Nil
        }
      }.toArray
    }
  }
  object IntermediatePresentation {
    // utility
    // accum(List(3, 4, 2)) == List(0, 3, 7)
    private def accum(lengths: List[Int]): List[Int] =
      lengths.foldLeft(List(0))((acc, n) => acc.head + n :: acc).tail.reverse

    sealed trait Elem {
      def bytes: Int // コンパイル後のオペコードんバイト数
      def prettyName: String
    }
    object Elem {
      sealed abstract class Operation(val code: Byte, val prettyName: String) extends Elem {
        override val bytes: Int = 1
      }
      object Operation {
        object Iconst_1 extends Operation(Operations.iconst_1, "iconst_1")
        object Iload_0 extends Operation(Operations.iload_0, "iload_0")
        object Iload_1 extends Operation(Operations.iload_1, "iload_1")
        object Iload_2 extends Operation(Operations.iload_2, "iload_2")
        object Istore_0 extends Operation(Operations.istore_0, "istore_0")
        object Istore_1 extends Operation(Operations.istore_1, "istore_1")
        object Istore_2 extends Operation(Operations.istore_2, "istore_2")
        object Bipush extends Operation(Operations.bipush, "bipush")
        object Iadd extends Operation(Operations.iadd, "iadd")
        object Ireturn extends Operation(Operations.ireturn, "ireturn")
        object Invokestatic extends Operation(Operations.invokestatic, "invokestatic")
      }
      case class ConstValue(code: Byte) extends Elem {
        override val bytes: Int = 1
        override val prettyName: String = code.toString
      }
      case class FunctionRef(name: String) extends Elem {
        override val bytes: Int = 2
        override val prettyName: String = s"[program address of $name()]"
      }
    }
    case class FunctionDef(name: String, elems: List[Elem])
  }

}
