import ast._
import ast.operation._

object Source extends App {

  try {
    println(Checker.apply(BinaryOperation(BinaryOperation(Bool(true), Sub, Const(1)), Add, Const(2))))
  } catch {
    case e: Exception => println(e)
  }
  try {
    Checker.apply(If(BinaryOperation(Const(1), Eq, Const(1)), Const(1), Bool(true)))
  } catch {
    case e: Exception => println(e)
  }
  try {
    println(Checker.apply(If(BinaryOperation(Const(1), Add, Const(1)), Const(1), Const(2))))
  } catch {
    case e: Exception => println(e)
  }
  println(Checker.apply(If(BinaryOperation(Const(1), Eq, Const(1)), Const(1), Const(2))))
  println(Checker.apply(If(BinaryOperation(Const(1), Eq, Const(1)), Bool(true), Bool(false))))
}
