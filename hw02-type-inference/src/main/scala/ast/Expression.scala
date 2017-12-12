package ast

import ast.operation.Operation

sealed trait Type

final case class IntType() extends Type

final case class BoolType() extends Type

final case class FunctionType(argumentsTypes: List[Type], resultType: Type) extends Type

sealed trait Expression

sealed trait Value extends Expression

final case class Const(value: Int) extends Value

final case class Val(value: String) extends Value

final case class Bool(value: Boolean) extends Value

final case class If(condition: Expression, ifThen: Expression, elseIf: Expression) extends Expression

final case class Lambda(arguments: List[String], body: Expression) extends Expression

final case class Apply(lambda: Expression, parameters: Map[String, Expression]) extends Expression

final case class ValDecl(variable: Map[String, Expression], body: Expression) extends Expression

final case class BinaryOperation(l: Expression, operation: Operation, r: Expression) extends Expression

object Expression {
  def apply(i: Int): Const = Const(i)

  def apply(s: String): Val = Val(s)

  def apply(i: Expression, t: Expression, e: Expression): If = If(i, t, e)

  def apply(arguments: List[String], body: Expression): Lambda = Lambda(arguments, body)

  def apply(expression: Expression, parameters: Map[String, Expression]): Apply = Apply(expression, parameters)

  def apply(value: Map[String, Expression], body: Expression): ValDecl = ValDecl(value, body)

  def apply(l: Expression, o: Operation, r: Expression): BinaryOperation = BinaryOperation(l, o, r)
}


