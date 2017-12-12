import ast._
import ast.operation._

object Source extends App {

  BinaryOperationsTest
  IfTest
  LambdaTest
  valDeclTest

  private def valDeclTest = {
    Console.println(Checker.apply(ValDecl(Map(Val("x") -> BinaryOperation(Const(3), Add, Const(4))), BinaryOperation(Const(2), Add, Val("x")))))
    Console.println(Checker.apply(ValDecl(Map(Val("x") -> BinaryOperation(Const(3), Add, Const(4))), BinaryOperation(Const(2), Eq, Val("x")))))

    try {
      val valDecl = ValDecl(Map(
        Val("x") -> BinaryOperation(Const(3), Add, Const(4)),
        Val("y") -> Lambda(Map(Val("n") -> new IntType, Val("z") -> new IntType), BinaryOperation(Val("n"), Add, Val("z")))), BinaryOperation(Val("y"), Add, Val("x")))
      Console.println(Checker.apply(valDecl))
    } catch {
      case e: Exception => println(e)
    }
  }

  private def LambdaTest = {
    try {
      println(Checker.apply(Lambda(Map(Val("n") -> new IntType, Val("x") -> new BoolType), BinaryOperation(Val("n"), Add, Val("x")))))
    } catch {
      case e: Exception => println(e)
    }
    try {
      println(Checker.apply(Lambda(Map(Val("n") -> new IntType, Val("x") -> new IntType), BinaryOperation(Val("n"), Add, BinaryOperation(Val("n"), Eq, Val("x"))))))
    } catch {
      case e: Exception => println(e)
    }
    println(Checker.apply(Lambda(Map(Val("n") -> new IntType, Val("x") -> new IntType), BinaryOperation(Val("n"), Add, Val("x")))))
    println(Checker.apply(Lambda(Map(Val("n") -> new IntType, Val("x") -> new IntType), BinaryOperation(Val("n"), Add, BinaryOperation(Val("n"), Sub, Val("x"))))))
  }

  private def BinaryOperationsTest = {
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
  }

  private def IfTest = {
    try {
      println(Checker.apply(If(BinaryOperation(Const(1), Add, Const(1)), Const(1), Const(2))))
    } catch {
      case e: Exception => println(e)
    }
    println(Checker.apply(If(BinaryOperation(Const(1), Eq, Const(1)), Const(1), Const(2))))
    println(Checker.apply(If(BinaryOperation(Const(1), Eq, Const(1)), Bool(true), Bool(false))))
    println(Checker.apply(If(BinaryOperation(Const(1), Eq, BinaryOperation(Const(1), Add, Const(0))), Const(1), Const(2))))
  }
}
