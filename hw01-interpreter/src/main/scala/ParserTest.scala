
object ParserTest extends App {
  val ast = Parser.parseAll(Parser.program, "main : if(false) then 5 else 6")
  ast match {
    case Parser.Success(result, _) => println(Interpreter.apply(result))
    case _: Parser.NoSuccess => println("Error")
  }

  val ast1 = Parser.parseAll(Parser.program, "main : 5")
  ast1 match {
    case Parser.Success(result, _) => println(Interpreter.apply(result))
    case _: Parser.NoSuccess => println("Error")
  }

  val ast2 = Parser.parseAll(Parser.program, "main : true")
  ast2 match {
    case Parser.Success(result, _) => println(Interpreter.apply(result))
    case _: Parser.NoSuccess => println("Error")
  }

  val ast3 = Parser.parseAll(Parser.program, "main : let a = 5 in a + 1")
  ast3 match {
    case Parser.Success(result, _) => println(Interpreter.apply(result))
    case _: Parser.NoSuccess => println("Error")
  }

  val ast4 = Parser.parseAll(Parser.program, "main : (a,b)->{a+b}")
  ast4 match {
    case Parser.Success(result, _) => println(Interpreter.apply(result))
    case _: Parser.NoSuccess => println("Error")
  }

  val ast5 = Parser.parseAll(Parser.program, "main : let f = (a,b)->{a+b} in f(a=6, b=3)")
  ast5 match {
    case Parser.Success(result, _) => println(Interpreter.apply(result))
    case x: Parser.Failure => throw new RuntimeException(x.toString())
    case x: Parser.Error => throw new RuntimeException(x.toString())
  }
}
