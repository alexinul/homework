package ast

import ast.operation._

sealed trait Expression

sealed trait Value extends Expression

final case class Const(value: Int) extends Value {
  def this(value: String) = this(value.toInt)
}

final case class Val(value: String) extends Value

final case class Bool(value: Boolean) extends Value {
  def this(value: String) = this(value.toBoolean)
}

final case class If(condition: Expression, ifThen: Expression, elseIf: Expression) extends Expression

final case class Lambda(arguments: List[String], body: Expression) extends Expression

final case class Apply(lambda: Expression, parameters: Map[String, Expression]) extends Expression

final case class ValDecl(variable: Map[String, Expression], body: Expression) extends Expression

final case class BinaryOperation(l: Expression, operation: Operation, r: Expression) extends Expression {
  def this(l: Expression, operation: String, r: Expression) = this(l,
    operation match {
      case o if o.equals("+") => Add
      case o if o.equals("-") => Sub
      case o if o.equals("/") => Div
      case o if o.equals("*") => Mul
      case o if o.equals("<") => LT
      case o if o.equals("<=") => LTE
      case o if o.equals(">") => GT
      case o if o.equals(">=") => GTE
      case o if o.equals("==") => Eq
      case o if o.equals("!=") => NEq
    }, r)
}

object Expression {
  def apply(i: Int): Const = Const(i)

  def apply(s: String): Val = Val(s)

  def apply(i: Expression, t: Expression, e: Expression): If = If(i, t, e)

  def apply(arguments: List[String], body: Expression): Lambda = Lambda(arguments, body)

  def apply(expression: Expression, parameters: Map[String, Expression]): Apply = Apply(expression, parameters)

  def apply(value: Map[String, Expression], body: Expression): ValDecl = ValDecl(value, body)

  def apply(l: Expression, o: Operation, r: Expression): BinaryOperation = BinaryOperation(l, o, r)
}



