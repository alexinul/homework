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

final case class Lambda(arguments: Map[Val, Type], body: Expression) extends Expression

final case class Apply(lambda: Expression, parameters: Map[Val, Expression]) extends Expression

final case class ValDecl(variable: Map[Val, Expression], body: Expression) extends Expression

final case class BinaryOperation(l: Expression, operation: Operation, r: Expression) extends Expression

