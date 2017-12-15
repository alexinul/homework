import ast._
import ast.operation._

object Source extends App {

  println("Binary Operation Test")
  println("---------------------------------------------------")
  BinaryOperationsTest

  println("---------------------------------------------------")
  println("If Test")
  println("---------------------------------------------------")
  IfTest

  println("---------------------------------------------------")
  println("Lambda Test")
  println("---------------------------------------------------")
  LambdaTest

  println("---------------------------------------------------")
  println("ValDecl Test")
  println("---------------------------------------------------")
  valDeclTest

  println("---------------------------------------------------")
  println("Apply Test")
  println("---------------------------------------------------")
  applyTest

  println("---------------------------------------------------")
  println("Rec Test")
  println("---------------------------------------------------")
  recTest

  println("---------------------------------------------------")
  println("Factorial Test")
  println("---------------------------------------------------")
  factorialTest

  println("---------------------------------------------------")
  println("Fibonacci Test")
  println("---------------------------------------------------")
  fibonacciTest

  private def recTest = {
    val ans1 = Checker.typeOf(ValDecl(
      Map(Val("rec") -> Lambda(List(), {
        BinaryOperation(Apply(Val("rec")), Add, Const(1))
      })), Apply(Val("rec"))
    ))
    println(Checker.typeToExternalForm(ans1.ty, ans1.subst))

    val ans2 = Checker.typeOf(ValDecl(
      Map(Val("rec") -> Lambda(List(), {
        BinaryOperation(Const(1), Add, Const(1))
      })), Apply(Val("rec"))
    ))
    println(Checker.typeToExternalForm(ans2.ty, ans2.subst))
  }

  private def fibonacciTest = {
    val fib = ValDecl(
      Map(Val("fib") -> Lambda(List(Val("n")), {
        If(BinaryOperation(Val("n"), Eq, Const(0)),
          Const(0),
          If(BinaryOperation(Val("n"), Eq, Const(1)),
            Const(1),
            BinaryOperation(
              Apply(Val("fib"), Map(Val("n") -> BinaryOperation(Val("n"), Sub, Const(1)))),
              Add,
              Apply(Val("fib"), Map(Val("n") -> BinaryOperation(Val("n"), Sub, Const(2))))))
        )
      })), Apply(Val("fib"), Map(Val("n") -> Const(7)))
    )
    val ans = Checker.typeOf(fib);
    Console.println(Checker.typeToExternalForm(ans.ty, ans.subst))
  }

  private def factorialTest = {
    val fact5 = ValDecl(
      Map(Val("fact") -> Lambda(List(Val("n")), {
        If(BinaryOperation(Val("n"), Eq, Const(0)),
          Const(1),
          BinaryOperation(Val("n"), Mul, Apply(Val("fact"), Map(Val("n") -> BinaryOperation(Val("n"), Sub, Const(1))))))
      })), Apply(Val("fact"), Map(Val("n") -> Const(5)))
    )

    val ans = Checker.typeOf(fact5)

    println(Checker.typeToExternalForm(ans.ty, ans.subst))
  }


  private def applyTest = {
    val lambdaBO = Lambda(List(Val("n")), BinaryOperation(Val("n"), Add, Const(2)))

    val ansBO = Checker.typeOf(Apply(lambdaBO, Map(Val("n") -> BinaryOperation(Const(2), Mul, Const(2)))))
    println(Checker.typeToExternalForm(ansBO.ty, ansBO.subst))

    val lambdaIf = Lambda(List(Val("n"), Val("y")), If(BinaryOperation(Const(1), Eq, Val("n")), Val("y"), Const(321)))
    val ansIf = Checker.typeOf(Apply(lambdaIf, Map(Val("n") -> Const(1), Val("y") -> Const(123))))
    println(Checker.typeToExternalForm(ansIf.ty, ansIf.subst))
  }

  private def valDeclTest = {
    val ans1 = Checker.typeOf(ValDecl(Map(Val("x") -> BinaryOperation(Const(3), Add, Const(4))), BinaryOperation(Const(2), Add, Val("x"))))
    println(Checker.typeToExternalForm(ans1.ty, ans1.subst))

    val ans2 = Checker.typeOf(ValDecl(Map(Val("x") -> BinaryOperation(Const(3), Add, Const(4))), BinaryOperation(Const(2), Eq, Val("x"))))
    println(Checker.typeToExternalForm(ans2.ty, ans2.subst))

    val valDecl = ValDecl(Map(
      Val("x") -> BinaryOperation(Const(3), Add, Const(4)),
      Val("y") -> Lambda(List(Val("n"), Val("z")), BinaryOperation(Val("n"), Add, Val("z")))), BinaryOperation(Val("y"), Add, Val("x")))
    val ans = Checker.typeOf(valDecl)
    println(Checker.typeToExternalForm(ans.ty, ans.subst))
  }

  private def LambdaTest = {
    val ans0 = Checker.typeOf(Lambda(List(Val("n"), Val("x")), BinaryOperation(Val("n"), Add, Val("x"))))
    println(Checker.typeToExternalForm(ans0.ty, ans0.subst))

    val ans1 = Checker.typeOf(Lambda(List(Val("x"), Val("y"), Val("z")), If(Val("x"), Val("y"), Val("z"))))
    println(Checker.typeToExternalForm(ans1.ty, ans1.subst))

    val ansLambdaBO = Checker.typeOf(Lambda(List(Val("n")), BinaryOperation(Val("n"), Add, Const(2))))
    println(Checker.typeToExternalForm(ansLambdaBO.ty, ansLambdaBO.subst))

    val lambdaIf = Lambda(List(Val("n"), Val("y")), If(Val("n"), Val("y"), Const(321)))
    val ans2 = Checker.typeOf(Apply(lambdaIf, Map(Val("n") -> Bool(true), Val("y") -> Const(123))))
    println(Checker.typeToExternalForm(ans2.ty, ans2.subst))

    try {
      val ans3 = Checker.typeOf(Lambda(List(Val("n"), Val("x")), BinaryOperation(Val("n"), Add, BinaryOperation(Val("n"), Eq, Val("x")))))
      println(Checker.typeToExternalForm(ans3.ty, ans3.subst))
    } catch {
      case e: Exception => println(e)
    }
    val ans4 = Checker.typeOf(Lambda(List(Val("n"), Val("x")), BinaryOperation(Val("n"), Add, Val("x"))))
    println(Checker.typeToExternalForm(ans4.ty, ans4.subst))

    val ans5 = Checker.typeOf(Lambda(List(Val("n"), Val("x")), BinaryOperation(Val("n"), Add, BinaryOperation(Val("n"), Sub, Val("x")))))
    println(Checker.typeToExternalForm(ans5.ty, ans5.subst))

    val ans6 = Checker.typeOf(Lambda(List(Val("n")), Val("n")))
    println(Checker.typeToExternalForm(ans6.ty, ans6.subst))
  }

  private def BinaryOperationsTest = {
    try {
      val ans1 = Checker.typeOf(BinaryOperation(BinaryOperation(Bool(true), Sub, Const(1)), Add, Const(2)))
      println(Checker.typeToExternalForm(ans1.ty, ans1.subst))
    } catch {
      case e: Exception => println(e)
    }
    try {
      val ans2 = Checker.typeOf(If(BinaryOperation(Const(1), Eq, Const(1)), Const(1), Bool(true)))
      println(Checker.typeToExternalForm(ans2.ty, ans2.subst))
    } catch {
      case e: Exception => println(e)
    }

    val ans3 = Checker.typeOf(If(BinaryOperation(Const(1), Eq, Const(1)), Const(1), Const(0)))
    println(Checker.typeToExternalForm(ans3.ty, ans3.subst))

    val ans4 = Checker.typeOf(BinaryOperation(BinaryOperation(Const(1), Sub, Const(1)), Add, Const(2)))
    println(Checker.typeToExternalForm(ans4.ty, ans4.subst))
  }

  private def IfTest = {
    try {
      val ans1 = Checker.typeOf(If(BinaryOperation(Const(1), Add, Const(1)), Const(1), Const(2)))
      println(Checker.typeToExternalForm(ans1.ty, ans1.subst))
    } catch {
      case e: Exception => println(e)
    }
    val ans2 = Checker.typeOf(If(BinaryOperation(Const(1), Eq, Const(1)), Const(1), Const(2)))
    println(Checker.typeToExternalForm(ans2.ty, ans2.subst))

    val ans3 = Checker.typeOf(If(BinaryOperation(Const(1), Eq, Const(1)), Bool(true), Bool(false)))
    println(Checker.typeToExternalForm(ans3.ty, ans3.subst))

    val ans4 = Checker.typeOf(If(BinaryOperation(Const(1), Eq, BinaryOperation(Const(1), Add, Const(0))), Const(1), Const(2)))
    println(Checker.typeToExternalForm(ans4.ty, ans4.subst))
  }
}
