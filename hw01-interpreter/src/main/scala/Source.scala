import ast._
import ast.operation._

object Source extends App {
  simpleOperationsTest
  lambdaTest
  valDeclTest
  factorialTest
  fibonacciTest

  private def fibonacciTest = {
    val fib = ValDecl(
      Map("fib" -> Lambda(List("n"), {
        If(BinaryOperation(Val("n"), Eq, Const(0)),
          Const(0),
          If(BinaryOperation(Val("n"), Eq, Const(1)),
            Const(1),
            BinaryOperation(
              Apply(Val("fib"), Map("n" -> BinaryOperation(Val("n"), Sub, Const(1)))),
              Add,
              Apply(Val("fib"), Map("n" -> BinaryOperation(Val("n"), Sub, Const(2))))))
        )
      })), Apply(Val("fib"), Map("n" -> Const(7)))
    )
    Console.println(Interpreter.apply(fib))
  }

  private def factorialTest = {
    val fact5 = ValDecl(
      Map("fact" -> Lambda(List("n"), {

        If(BinaryOperation(Val("n"), Eq, Const(0)),
          Const(1),
          BinaryOperation(Val("n"), Mul, Apply(Val("fact"), Map("n" -> BinaryOperation(Val("n"), Sub, Const(1))))))
      })), Apply(Val("fact"), Map("n" -> Const(5)))
    )
    Console.println(Interpreter.apply(fact5))
  }

  private def simpleOperationsTest = {
    val add12 = BinaryOperation(BinaryOperation(Const(3), Add, Const(4)), Sub, Const(2))
    val addA3 = BinaryOperation(Val("A"), Add, Const(1))
    val eq55 = BinaryOperation(add12, Eq, Const(5))
    val eq56 = BinaryOperation(add12, Eq, Const(6))
    val one = Const(1)
    val a = Val("1")
    val eqaa = BinaryOperation(a, Eq, one);
    val `if` = If(BinaryOperation(one, Eq, one), add12, eq55)
    val xAddy2 = BinaryOperation(Val("x"), Mul, Val("y"))
    val xAddy = BinaryOperation(Val("x"), Add, Val("y"))
    val oneAddLambda = BinaryOperation(one, Add, Lambda(List("n"), BinaryOperation(Val("n"), Sub, Const(1))))

    Console.println(add12.toString + "|" + Interpreter.apply(add12))
    try {
      Console.println(addA3.toString + "|" + Interpreter.apply(addA3))
    } catch {
      case e: Exception => Console.println(e)
    }
    Console.println(eq55.toString + "|" + Interpreter.apply(eq55))
    Console.println(eq56.toString + "|" + Interpreter.apply(eq56))
    Console.println(one.toString + "|" + Interpreter.apply(one))
    Console.println(a.toString + "|" + Interpreter.apply(a))
    try {
      Console.println(eqaa.toString + "|" + Interpreter.apply(eqaa))
    } catch {
      case e: Exception => Console.println(e)
    }
    Console.println(`if`.toString + "|" + Interpreter.apply(`if`))
    Console.println(Interpreter.apply(xAddy, Map("x" -> Const(1), "y" -> Const(2))))
    Console.println(Interpreter.apply(xAddy2, Map("x" -> Const(2), "y" -> BinaryOperation(Const(3), Add, Const(2)))))
    try {
      Console.println(Interpreter.apply(oneAddLambda))
    } catch {
      case e: Exception => Console.println(e)
    }
  }

  private def lambdaTest = {
    val lambdaBO = Lambda(List("n"), BinaryOperation(Val("n"), Add, Const(2)))
    Console.println(Interpreter.apply(lambdaBO))
    Console.println(Interpreter.apply(Apply(lambdaBO, Map("n" -> BinaryOperation(Val("x"), Mul, Val("y")))), Map("x" -> Const(2), "y" -> Const(3))))

    val lambdaIf = Lambda(List("n", "y"), If(BinaryOperation(Const(1), Eq, Val("n")), Val("y"), Const(321)))
    Console.println(Interpreter.apply(lambdaIf))
    Console.println(Interpreter.apply(Apply(lambdaIf, Map("n" -> Const(1), "y" -> Const(123)))))
  }

  private def valDeclTest = {
    val valDecl = ValDecl(Map("x" -> BinaryOperation(Const(3), Add, Const(4))), BinaryOperation(Const(2), Add, Val("x")))
    Console.println(Interpreter.apply(valDecl))

    val valDecl2 = ValDecl(Map(
      "x" -> BinaryOperation(Const(3), Add, Const(4)),
      "y" -> Lambda(List("n"), BinaryOperation(Val("n"), Add, Val("x")))),
      BinaryOperation(Apply(Val("y"), Map("n" -> Const(1))), Add, Val("x")))
    Console.println(Interpreter.apply(valDecl2))
  }
}
