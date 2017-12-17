import ast._
import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  override val skipWhitespace: Boolean = true

  def factor: Parser[Expression] = {
    for {
      t <- expression | value | const | bool
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
      boolean <- """""true|false""".r
    } yield new Bool(boolean)
  }

  def expression: Parser[Expression] = {
    for {
      _ <- literal("(")
      exp <- literal("")
      _ <- literal(")")
    } yield exp
  }

  def binaryOperation: Parser[BinaryOperation] = {
    for {
      l <- factor
      op <- """+|-|*|/|<|<=|>|>=|==|!=""".r
      r <- factor
    } yield new BinaryOperation(l, op, r)
  }

  def ifOperation: Parser[If] = {
    for {
      _ <- literal("if")
      exp <- expression
      _ <- literal("{")
      ifThen <- expression
      _ <- literal("}")
      _ <- literal("{")
      elseIf <- expression
      _ <- literal("}")
    } yield If(exp, ifThen, elseIf)
  }

  def lambda: Parser[Lambda] = {
    for {
      _ <- literal("(")
      args <- rep(lambdaArguments)
      _ <- literal(")")
      _ <- literal("{")
      body <- expression
      _ <- literal("}")
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
      variables <- valBody
      body <- expression
    } yield ValDecl(variables, body)
  }

  def valBody: Parser[Map[String, Expression]] = {
    for {
      name <- ident
      body <- expression
      _ <- literal(",").?
    } yield Map(name -> body)
  }

  def apply: Parser[Apply] = {
    for {
      functionName <- value
      _ <- literal("(")
      arguments <- valBody
      _ <- literal(")")
    }
  }
}
