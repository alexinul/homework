import ast._
import ast.Type
import ast.operation._;

object Checker {

  def apply(program: Expression, environment: Map[Expression, Type] = Map()): Type = {
    program match {
      case Const(_) => new IntType
      case Bool(_) => new BoolType
      case BinaryOperation(l, operation, r) => checkBinaryOperation(l, operation, r, environment)
      case If(condition, ifThen, elseIf) => checkIf(condition, ifThen, elseIf, environment)
    }
  }

  def checkBinaryOperation(l: Expression, operation: Operation, r: Expression, environment: Map[Expression, Type]): Type =
    (apply(l, environment), apply(r, environment)) match {
      case (IntType(), IntType()) => operation match {
        case _: IntOperation => new IntType
        case _: BoolOperation => new BoolType
      }
      case _ => throw new RuntimeException("Binary Operations can be done only on integers")
    }

  def checkIf(condition: Expression, ifThen: Expression, elseIf: Expression, environment: Map[Expression, Type]): Type =
    apply(condition, environment) match {
      case BoolType() => if (apply(ifThen) == apply(elseIf)) apply(ifThen) else throw new RuntimeException("If cannot have a different result type on each branch")
      //TODO: improve " if (equalType(apply(ifThen), apply(elseIf))) apply(ifThen) "  I have to get rid of the third apply
      case _ => throw new RuntimeException("If must have a boolean as the condition")
    }


}
