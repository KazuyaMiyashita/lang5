package lang5.language

import lang5.language.Ast.*
import lang5.machine.IntermediatePresentation as IP
import lang5.machine.IntermediatePresentation.Elem.Operation as OP

import scala.util.control.Breaks

object LanguageCompiler {

  def compile(expression: Ast.Program): IP = {
    IP(expression.definitions.map { case FunctionDefinition(name, args, body) =>
      val locals: Array[String] = new Array(4) // 引数およびローカル変数に使うキーワード一覧。4つまで

      def setLocal(name: String): Int = {
        val b = new Breaks
        var idx = -1
        b.breakable {
          for (i <- 0 until 4) {
            if (locals(i) eq null) {
              locals(i) = name
              idx = i
              b.break()
            }
          }
        }
        if (idx == -1) {
          throw new Exception("the upper limit of function variables is 4...")
        }
        idx
      }
      def getLocal(name: String): Option[Int] = locals.zipWithIndex.find((n, _) => n == name).map(_._2)

      // 使われる引数・変数をまずローカル一覧に入れておく
      args.foreach(setLocal)
      body.collect { case a: Assignment => a }.map(_.name).foreach(setLocal)

      def toElems(expression: Expression): List[IP.Elem] = {
        expression match {
          case BinaryExpression(operator, lhs, rhs) =>
            toElems(lhs) ::: toElems(rhs) ::: (operator match {
              case Operator.Add      => OP.Iadd :: Nil
              case Operator.Multiply => OP.Imul :: Nil
            })

          case IntegerLiteral(value) =>
            value match {
              case 0 => OP.Iconst_0 :: Nil
              case 1 => OP.Iconst_1 :: Nil
              case 2 => OP.Iconst_2 :: Nil
              case 3 => OP.Iconst_3 :: Nil
              case 4 => OP.Iconst_4 :: Nil
              case 5 => OP.Iconst_5 :: Nil
              case n =>
                if (6 <= n && n <= 255) {
                  OP.Bipush :: IP.Elem.ConstValue(n.asInstanceOf[Byte]) :: Nil
                } else {
                  throw new Exception(s"integer literal must be between 0 and 255... value: $n")
                }
            }

          case Identifier(name) =>
            getLocal(name) match {
              case Some(0) => OP.Iload_0 :: Nil
              case Some(1) => OP.Iload_1 :: Nil
              case Some(2) => OP.Iload_2 :: Nil
              case Some(3) => OP.Iload_3 :: Nil
              case _       => throw new Exception("?")
            }

          case Assignment(name, expression) =>
            toElems(expression) :::
              (getLocal(name) match {
                case Some(0) => OP.Istore_0 :: Nil
                case Some(1) => OP.Istore_1 :: Nil
                case Some(2) => OP.Istore_2 :: Nil
                case Some(3) => OP.Istore_2 :: Nil
                case _       => throw new Exception("?")
              })

          case FunctionCall(name, args) =>
            args.flatMap(toElems) ::: OP.Invokestatic :: IP.Elem.FunctionRef(name) :: Nil
        }
      }

      val elems: List[IP.Elem] = body.flatMap(b => toElems(b))
      IP.FunctionDef(name, elems ::: OP.Ireturn :: Nil)
    })
  }

}
