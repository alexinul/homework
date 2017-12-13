import ast._
import ast.operation._

object Source extends App {

  //  BinaryOperationsTest
  //  IfTest
  //  LambdaTest
  //  valDeclTest
  //  applyTest
  //  factorialTest
  recTest

  //  Console.println(Checker.typeOf(ValDecl(Map(Val("x") -> BinaryOperation(Const(3), Add, Const(4))), BinaryOperation(Const(2), Add, Val("x")))))

  private def recTest = {
    println(Checker.typeToExternalForm(Checker.typeOf(ValDecl(
      Map(Val("rec") -> Lambda(Map(), {
        Val("rec")
      })), BinaryOperation(Const(1), Add, Const(2))
    )).ty))
  }

  private def factorialTest = {
    val fact5 = ValDecl(
      Map(Val("fact") -> Lambda(Map(Val("n") -> new IntType), {
        If(BinaryOperation(Val("n"), Eq, Const(0)),
          Const(1),
          BinaryOperation(Val("n"), Mul, Apply(Val("fact"), Map(Val("n") -> BinaryOperation(Val("n"), Sub, Const(1))))))
      })), Apply(Val("fact"), Map(Val("n") -> Const(5)))
    )
    Console.println(Checker.apply(fact5))
  }

  private def applyTest = {
    val lambdaBO = Lambda(Map(Val("n") -> new IntType), BinaryOperation(Val("n"), Add, Const(2)))
    Console.println(Checker.apply(Apply(lambdaBO, Map(Val("n") -> BinaryOperation(Val("x"), Mul, Val("y")))), Map(Val("x") -> new IntType, Val("y") -> new IntType)))

    val lambdaIf = Lambda(Map(Val("n") -> new IntType, Val("y") -> new IntType), If(BinaryOperation(Const(1), Eq, Val("n")), Val("y"), Const(321)))
    Console.println(Checker.apply(Apply(lambdaIf, Map(Val("n") -> Const(1), Val("y") -> Const(123)))))
  }

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
      println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new IntType, Val("x") -> new BoolType), BinaryOperation(Val("n"), Add, Val("x")))).ty))
    } catch {
      case e: Exception => println(e)
    }
    try {
      println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new IntType, Val("x") -> new IntType), BinaryOperation(Val("n"), Add, BinaryOperation(Val("n"), Eq, Val("x"))))).ty))
    } catch {
      case e: Exception => println(e)
    }
    println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new IntType, Val("x") -> new IntType), BinaryOperation(Val("n"), Add, Val("x")))).ty))
    println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new IntType, Val("x") -> new IntType), BinaryOperation(Val("n"), Add, BinaryOperation(Val("n"), Sub, Val("x"))))).ty))
    println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new IntType), Val("n"))).ty))
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
