object Source extends App {
  test3

  private def test2 = {
    val xAddy = BinaryOperation(Add, Val("x"), Val("y"))
    Console.println(interpreter.apply(xAddy, Map("x" -> Const(1), "y" -> Const(2))))

    val xAddy2 = BinaryOperation(Mul, Val("x"), Val("y"))
    Console.println(interpreter.apply(xAddy2, Map("x" -> Const(2), "y" -> BinaryOperation(Add, Const(3), Const(2)))))

  }

  private def test3 = {
    val fact5 = ValDecl(
      Map("fact" -> Lambda(List("n"), {
        If(Eq(Val("n"), Const(0)),
          Const(1),
          BinaryOperation(Mul, Val("n"), Apply(Val("fact"), Map("n" -> BinaryOperation(Sub, Val("n"), Const(1))))))
      })), Apply(Val("fact"), Map("n" -> Const(5)))
    )
    Console.println(interpreter.apply(fact5))
  }

  private def test1 = {
    val add12 = BinaryOperation(Sub, BinaryOperation(Add, Const(3), Const(4)), Const(2))
    val addA3 = BinaryOperation(Add, Val("A"), Const(1))
    val eq55 = Eq(add12, Const(5))
    val eq56 = Eq(add12, Const(6))
    val one = Const(1)
    val a = Val("1")
    val eqaa = Eq(a, one);
    val `if` = If(eqaa, add12, eq55)


    Console.println(add12.toString + interpreter.apply(add12))

    try {
      Console.println(addA3.toString + interpreter.apply(addA3))
    } catch {
      case e: Exception => Console.println(e)
    }

    Console.println(eq55.toString + "|" + interpreter.apply(eq55))
    Console.println(eq56.toString + "|" + interpreter.apply(eq56))
    Console.println(one.toString + "|" + interpreter.apply(one))
    Console.println(a.toString + "|" + interpreter.apply(a))
    Console.println(eqaa.toString + "|" + interpreter.apply(eqaa))
    Console.println(`if`.toString + "|" + interpreter.apply(`if`))
  }

}
