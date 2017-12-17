package ast

import ast.operation.Operation

sealed trait Type

final case class IntType() extends Type

final case class BoolType() extends Type

final case class FunctionType(argumentsTypes: List[Type], resultType: Type) extends Type

final case class VarType(sn: String) extends Type

final case class UnionType(ty1: Type, ty2: Type) extends Type


sealed trait OptionalType extends Type

final case class NoType() extends OptionalType

final case class AType(ty: Type) extends OptionalType


sealed trait Expression

sealed trait Value extends Expression

final case class Const(value: Int) extends Value

final case class Val(value: String) extends Value

final case class Bool(value: Boolean) extends Value

final case class If(condition: Expression, ifThen: Expression, elseIf: Expression) extends Expression

final case class Lambda(arguments: List[Val], body: Expression, optReturnType: Type = new NoType) extends Expression

final case class Apply(lambda: Expression, parameters: Map[Val, Expression] = Map()) extends Expression

final case class ValDecl(variable: Map[Val, Expression], body: Expression) extends Expression

final case class BinaryOperation(l: Expression, operation: Operation, r: Expression) extends Expression

