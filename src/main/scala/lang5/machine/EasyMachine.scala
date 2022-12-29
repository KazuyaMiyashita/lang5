package lang5.machine

import EasyMachine.*
import EasyMachine.Operation.Value
import scala.util.control.Breaks

class EasyMachine(val program: Array[Operation]) {

  var programCounter = 0

  val stack: Array[Int] = new Array(1024)
  var stackPtr: Int = 0
  var framePtr: Int = 0

  private def read(): Operation = {
    val op = program(programCounter)
    programCounter += 1
    op
  }

  private def push(v: Int): Unit = {
    stack(stackPtr) = v
    stackPtr += 1
  }

  private def pop(): Int = {
    stack(stackPtr) = 0
    stackPtr -= 1
    stack(stackPtr)
  }

  private def stackCopy(from: Int, to: Int): Unit = {
    stack(to) = stack(from)
  }

  private def getReturnAddr(): Int = {
    stack(framePtr + 2)
  }

  private def getArgc(): Int = {
    stack(framePtr)
  }

  private def getArgument(index: Int): Int = {
    stack(framePtr + index)
  }

  def run(): Array[Int] = {
    import Operation.*
    val b = new Breaks()
    b.breakable {
      while (true) {
        val op = read()
        op match {
          case Nop => ()
          case BiPush =>
            val value = read().asInstanceOf[Value].asInt
            push(value)
          case IAdd =>
            val a = pop()
            val b = pop()
            val v = (a.toInt + b.toInt).toByte
            push(v)
          case IConst_1 =>
            push(1)
          case v: Value => throw new Exception(s"Expecting to have an opcode, but has a value: $v")
          case InvokeVirtual =>
            val jumpAddr = read().asInstanceOf[ProgramAddress].asInt
            val argc = read().asInstanceOf[Value].asInt
            push(argc)
            val returnAddr = programCounter // InvokeVirtual の次の次にはジャンプ先が一つ設定されている。関数を抜けた後にread()が先に呼ばれて次のアドレスになる
            push(returnAddr)
            framePtr = stackPtr - 3 // FIXME returnAddr1個分, 引数2個分
            programCounter = jumpAddr
          case v: ProgramAddress => throw new Exception(s"Expecting to have an opcode, but has a program address: $v")
          case ILoad_0 =>
            val v = getArgument(0)
            push(v)
          case ILoad_1 =>
            val v = getArgument(1)
            push(v)
          case Return =>
            val returnAddr = getReturnAddr()
            programCounter = returnAddr
            framePtr = 0 // FIXME
            // スタックの配列が初期値に戻っていない（別に戻さなくてもいいけどpopと挙動が違う）
            System.arraycopy(stack, stackPtr - 1, stack, framePtr, stackPtr - 1 - framePtr)
            stackPtr = 0 // FIXME
          case Exit => b.break()
        }
      }
    }

    stack
  }

}

object EasyMachine {

  sealed trait Operation
  object Operation {
    object Nop extends Operation
    object IConst_1 extends Operation
    object BiPush extends Operation
    object IAdd extends Operation
    case class Value(asInt: Int) extends Operation
    object InvokeVirtual extends Operation
    case class ProgramAddress(asInt: Int) extends Operation
    object ILoad_0 extends Operation
    object ILoad_1 extends Operation
    object Return extends Operation
    object Exit extends Operation
  }

  // arg: (program index, operations) *
  def compile(iopss: (Int, Array[Operation])*): EasyMachine = {
    val program = Array.fill[Operation](1024)(Operation.Nop)
    iopss.foreach { case (index, ops) =>
      System.arraycopy(ops, 0, program, index, ops.length)
    }
    new EasyMachine(program)
  }

}
