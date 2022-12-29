package lang5.machine

// intermediate presentation

import IntermediatePresentation.*
import EasyMachine2.Operations

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

      object Iconst_0 extends Operation(Operations.iconst_0, "iconst_0")
      object Iconst_1 extends Operation(Operations.iconst_1, "iconst_1")
      object Iconst_2 extends Operation(Operations.iconst_2, "iconst_2")
      object Iconst_3 extends Operation(Operations.iconst_3, "iconst_3")
      object Iconst_4 extends Operation(Operations.iconst_4, "iconst_4")
      object Iconst_5 extends Operation(Operations.iconst_5, "iconst_5")

      object Iload_0 extends Operation(Operations.iload_0, "iload_0")
      object Iload_1 extends Operation(Operations.iload_1, "iload_1")
      object Iload_2 extends Operation(Operations.iload_2, "iload_2")
      object Iload_3 extends Operation(Operations.iload_3, "iload_3")

      object Istore_0 extends Operation(Operations.istore_0, "istore_0")
      object Istore_1 extends Operation(Operations.istore_1, "istore_1")
      object Istore_2 extends Operation(Operations.istore_2, "istore_2")
      object Istore_3 extends Operation(Operations.istore_3, "istore_3")

      object Bipush extends Operation(Operations.bipush, "bipush")

      object Iadd extends Operation(Operations.iadd, "iadd")

      object Imul extends Operation(Operations.imul, "imul")

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
