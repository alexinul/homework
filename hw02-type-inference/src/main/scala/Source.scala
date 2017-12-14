import ast._
import ast.operation._

object Source extends App {

  //  BinaryOperationsTest
  //  IfTest
  LambdaTest
  //  valDeclTest
  //  applyTest
  //  factorialTest
  //  recTest


  //  private def recTest = {
  //    println(Checker.typeToExternalForm(Checker.typeOf(ValDecl(
  //      Map(Val("rec") -> Lambda(Map(), {
  //        Val("rec")
  //      })), BinaryOperation(Const(1), Add, Const(2))
  //    )).ty))
  //  }
  //
  //  private def factorialTest = {
  //    val fact5 = ValDecl(
  //      Map(Val("fact") -> Lambda(Map(Val("n") -> new IntType), {
  //        If(BinaryOperation(Val("n"), Eq, Const(0)),
  //          Const(1),
  //          BinaryOperation(Val("n"), Mul, Apply(Val("fact"), Map(Val("n") -> BinaryOperation(Val("n"), Sub, Const(1))))))
  //      })), Apply(Val("fact"), Map(Val("n") -> Const(5)))
  //    )
  //    println(Checker.typeToExternalForm(Checker.typeOf(fact5).ty))
  //  }
  //
  //  private def applyTest = {
  //    val lambdaBO = Lambda(Map(Val("n") -> new IntType), BinaryOperation(Val("n"), Add, Const(2)))
  //    println(Checker.typeToExternalForm(Checker.typeOf(Apply(lambdaBO, Map(Val("n") -> BinaryOperation(Val("x"), Mul, Val("y")))), Map(Val("x") -> new IntType, Val("y") -> new IntType)).ty))
  //
  //    val lambdaIf = Lambda(Map(Val("n") -> new IntType, Val("y") -> new IntType), If(BinaryOperation(Const(1), Eq, Val("n")), Val("y"), Const(321)))
  //    println(Checker.typeToExternalForm(Checker.typeOf(Apply(lambdaIf, Map(Val("n") -> Const(1), Val("y") -> Const(123)))).ty))
  //  }
  //
  //  private def valDeclTest = {
  //    println(Checker.typeToExternalForm(Checker.typeOf(ValDecl(Map(Val("x") -> BinaryOperation(Const(3), Add, Const(4))), BinaryOperation(Const(2), Add, Val("x")))).ty))
  //    println(Checker.typeToExternalForm(Checker.typeOf(ValDecl(Map(Val("x") -> BinaryOperation(Const(3), Add, Const(4))), BinaryOperation(Const(2), Eq, Val("x")))).ty))
  //
  //    try {
  //      val valDecl = ValDecl(Map(
  //        Val("x") -> BinaryOperation(Const(3), Add, Const(4)),
  //        Val("y") -> Lambda(Map(Val("n") -> new IntType, Val("z") -> new IntType), BinaryOperation(Val("n"), Add, Val("z")))), BinaryOperation(Val("y"), Add, Val("x")))
  //      println(Checker.typeToExternalForm(Checker.typeOf(valDecl).ty))
  //    } catch {
  //      case e: Exception => println(e)
  //    }
  //  }

  private def LambdaTest = {
    //    val ans = Checker.typeOf(Lambda(List(Val("n"), Val("x")), BinaryOperation(Val("n"), Add, Val("x"))))
    //    println(Checker.typeToExternalForm(ans.ty, ans.subst))
    //
    //    val ans1 = Checker.typeOf(Lambda(List(Val("x"), Val("y"), Val("z")), If(Val("x"), Val("y"), Val("z"))))
    //    println(Checker.typeToExternalForm(ans1.ty, ans1.subst))

    val lambdaIf = Lambda(List(Val("n"), Val("y")), If(Val("n"), Val("y"), Const(321)))
    val ans = Checker.typeOf(Apply(lambdaIf, Map(Val("n") -> Bool(true), Val("y") -> Const(123))))
    println(Checker.typeToExternalForm(ans.ty, ans.subst))

    //    try {
    //      println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new NoType, Val("x") -> new NoType), BinaryOperation(Val("n"), Add, BinaryOperation(Val("n"), Eq, Val("x"))))).ty))
    //    } catch {
    //      case e: Exception => println(e)
    //    }
    //    println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new NoType, Val("x") -> new NoType), BinaryOperation(Val("n"), Add, Val("x")))).ty))
    //    println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new NoType, Val("x") -> new NoType), BinaryOperation(Val("n"), Add, BinaryOperation(Val("n"), Sub, Val("x"))))).ty))
    //    println(Checker.typeToExternalForm(Checker.typeOf(Lambda(Map(Val("n") -> new NoType), Val("n"))).ty))
  }

  private def BinaryOperationsTest = {
    try {
      println(Checker.typeToExternalForm(Checker.typeOf(BinaryOperation(BinaryOperation(Bool(true), Sub, Const(1)), Add, Const(2))).ty))
    } catch {
      case e: Exception => println(e)
    }
    try {
      println(Checker.typeToExternalForm(Checker.typeOf(If(BinaryOperation(Const(1), Eq, Const(1)), Const(1), Bool(true))).ty))
    } catch {
      case e: Exception => println(e)
    }

  }

  private def IfTest = {
    try {
      println(Checker.typeToExternalForm(Checker.typeOf(If(BinaryOperation(Const(1), Add, Const(1)), Const(1), Const(2))).ty))
    } catch {
      case e: Exception => println(e)
    }
    println(Checker.typeToExternalForm(Checker.typeOf(If(BinaryOperation(Const(1), Eq, Const(1)), Const(1), Const(2))).ty))
    println(Checker.typeToExternalForm(Checker.typeOf(If(BinaryOperation(Const(1), Eq, Const(1)), Bool(true), Bool(false))).ty))
    println(Checker.typeToExternalForm(Checker.typeOf(If(BinaryOperation(Const(1), Eq, BinaryOperation(Const(1), Add, Const(0))), Const(1), Const(2))).ty))
  }
}
