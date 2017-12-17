import ast._
import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  override val skipWhitespace: Boolean = true

  def factor: Parser[Expression] = {
    for {
      t <- const | bool | value
    } yield t
  }

  def value: Parser[Val] = {
    for {
      name <- ident
    } yield Val(name)
  }

  def const: Parser[Const] = {
    for {
      number <- wholeNumber
    } yield new Const(number)
  }

  def bool: Parser[Bool] = {
    for {
      boolean <- literal("true") | literal("false")
    } yield new Bool(boolean)
  }

  def program: Parser[Expression] = {
    for {
      _ <- literal("main")
      _ <- literal(":")
      exp <- expression
    } yield exp
  }

  def expression: Parser[Expression] = {
    for {
      exp <- valDecl | ifOperation | binaryOperation | lambda | apply | factor
    } yield exp
  }

  def binaryOperation: Parser[BinaryOperation] = {
    for {
      _ <- literal("{")
      l <- ifOperation | binaryOperation | apply | factor
      op <- literal("+") | literal("-") | literal("*") | literal("/") | literal("<") | literal("<=") | literal(">") | literal(">=") | literal("==") | literal("!=")
      r <- ifOperation | binaryOperation | apply | factor
      _ <- literal("}")
    } yield new BinaryOperation(l, op, r)
  }

  def ifOperation: Parser[If] = {
    for {
      _ <- literal("if")
      _ <- literal("(")
      exp <- expression
      _ <- literal(")")
      _ <- literal("then")
      ifThen <- expression
      _ <- literal("else")
      elseIf <- expression
    } yield If(exp, ifThen, elseIf)
  }

  def lambda: Parser[Lambda] = {
    for {
      _ <- literal("(")
      args <- rep(lambdaArguments)
      _ <- literal(")")
      _ <- literal("->")
      body <- expression
    } yield Lambda(args, body)
  }

  def lambdaArguments: Parser[String] = {
    for {
      arg <- ident
      _ <- literal(",").?
    } yield arg
  }

  def valDecl: Parser[ValDecl] = {
    for {
      _ <- literal("let")
      variables <- rep(valBody)
      _ <- literal("in")
      body <- expression
    } yield ValDecl(variables.reduceLeft(_ ++ _), body)
  }

  def valBody: Parser[Map[String, Expression]] = {
    for {
      name <- ident
      _ <- literal("=")
      body <- apply | expression
      _ <- literal(",").?
    } yield Map(name -> body)
  }

  def apply: Parser[Apply] = {
    for {
      functionName <- value
      _ <- literal("(")
      arguments <- rep(valBody)
      _ <- literal(")")
    } yield Apply(functionName, arguments.reduceLeft(_ ++ _))
  }
}
