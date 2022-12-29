package lang5.machine

import EasyMachine.*
import EasyMachine.Operation.{Value, ProgramAddress}
import org.scalatest.funsuite.AnyFunSuite

class EasyMachineTest extends AnyFunSuite {

  test("be good 1") {
    val easyMachine = {
      import Operation.*
      EasyMachine.compile(
        0 -> Array(IConst_1, BiPush, Value(42), IAdd, Exit)
      )
    }
    val result = easyMachine.run()
    assert(result.toList.head == 43)
  }

  test("be good 2") {
    val easyMachine = {
      import Operation.*
      EasyMachine.compile(
        0 -> Array(IConst_1, BiPush, Value(42), InvokeVirtual, ProgramAddress(20), Exit),
        20 -> Array(ILoad_0, ILoad_1, IAdd, Return)
      )

    }
    val result = easyMachine.run()
    assert(result.toList.head == 43)
  }

  test("be good 3") {
    val easyMachine = {
      import Operation.*
      // (1 + 42) + 3
      EasyMachine.compile(
        0 -> Array(IConst_1, BiPush, Value(42), InvokeVirtual, ProgramAddress(20), Exit),
        20 -> Array(ILoad_0, ILoad_1, IAdd, InvokeVirtual, ProgramAddress(40), Return),
        40 -> Array(ILoad_0, BiPush, Value(3), IAdd, Return)
      )

    }
    val result = easyMachine.run()
    assert(result.toList.head == 43)
  }

}
