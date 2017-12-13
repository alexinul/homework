import ast._
import ast.Type
import ast.operation._

import scala.util.Random

object Checker {

  def apply(program: Expression, environment: Map[Expression, Type] = Map()): Type = {
    program match {
      case Const(_) => new IntType
      case Bool(_) => new BoolType
      case ref@Val(_) => environment.get(ref) match {
        case Some(value) => value
        case None => throw new RuntimeException("Val was not declared")
      }
      case BinaryOperation(l, operation, r) => checkBinaryOperation(l, operation, r, environment)
      case If(condition, ifThen, elseIf) => checkIf(condition, ifThen, elseIf, environment)
      case Lambda(arguments, body, _) => checkLambda(arguments, body, environment)
      case ValDecl(variable, body, _, _) => checkValDecl(variable, body, environment)
      case Apply(lambda, parameters) => checkApply(lambda, parameters, environment)
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
      case BoolType() => if (apply(ifThen, environment) == apply(elseIf, environment)) apply(ifThen, environment) else throw new RuntimeException("If cannot have a different result type on each branch")
      //TODO: improve " if (equalType(apply(ifThen), apply(elseIf))) apply(ifThen) "  I have to get rid of the third apply
      case _ => throw new RuntimeException("If must have a boolean as the condition")
    }

  def checkLambda(arguments: Map[Val, Type], body: Expression, environment: Map[Expression, Type]): Type =
    FunctionType(arguments.map(_._2).toList, apply(body, environment ++ arguments))

  def checkValDecl(variable: Map[Val, Expression], body: Expression, environment: Map[Expression, Type]): Type =
    apply(body, environment ++ variable.map { case (k: Val, v: Expression) => (k, apply(v, environment)) })

  def checkApply(lambda: Expression, parameters: Map[Val, Expression], environment: Map[Expression, Type]): Type = {
    apply(lambda, environment ++ parameters.map { case (k: Val, v: Expression) => (k, apply(v, environment)) })
  }

  def applyOneSubst(ty0: Type, tvar: VarType, ty1: Type): Type = {
    ty0 match {
      case self@(IntType() | BoolType()) => self
      case FunctionType(argumentsTypes, resultType) => FunctionType(argumentsTypes.map(argType => applyOneSubst(argType, tvar, ty1)), applyOneSubst(resultType, tvar, ty1))
      case VarType(_) => if (ty0 != tvar) ty1 else ty0
    }
  }

  def applySubstToType(ty: Type, subst: Map[Type, VarType]): Type = {
    ty match {
      case self@(IntType() | BoolType()) => self
      case FunctionType(argumentsTypes, resultType) => FunctionType(argumentsTypes.map(argType => applySubstToType(argType, subst)), applySubstToType(resultType, subst))
      case VarType(_) => subst.get(ty) match {
        case Some(value) => value
        case None => ty
      }
    }
  }

  def emptySubst(): Map[Type, VarType] = Map.empty

  def extendSubst(subst: Map[Type, VarType], tvar: VarType, ty: Type): Map[Type, VarType] = subst.map { case (k, v) => (k, applyOneSubst(v, tvar, ty).asInstanceOf[VarType]) } + (ty -> tvar)

  def unifier(ty1: Type, ty2: Type, subst: Map[Type, VarType], exp: Expression): Map[Type, VarType] = {
    val ty11 = applySubstToType(ty1, subst)
    val ty22 = applySubstToType(ty2, subst)
    if (ty11 == ty22) subst
    else if (ty11.isInstanceOf[VarType]) {
      ty11 match {
        case self@VarType(_) => if (noOccurrence(self, ty22)) extendSubst(subst, self, ty22) else throw new RuntimeException("Cannot find type")
      }
    } else if (ty22.isInstanceOf[VarType]) {
      ty22 match {
        case self@VarType(_) => if (noOccurrence(self, ty11)) extendSubst(subst, self, ty11) else throw new RuntimeException("Cannot find type")
      }
    }
    else if (ty11 == FunctionType && ty22 == FunctionType) {
      (ty11, ty22) match {
        case (FunctionType(argTy1, resTy1), FunctionType(argTy2, resTy2)) => {
          (argTy1 zip argTy2).map { case (arg1, arg2) => unifier(arg1, arg2, subst, exp) }
          unifier(resTy1, resTy2, subst, exp)
        }
      }
    } else {
      throw new RuntimeException("Cannot find type")
    }
  }

  def noOccurrence(tvar: VarType, ty: Type): Boolean = {
    ty match {
      case IntType() => true
      case BoolType() => true
      case FunctionType(argumentsTypes, resultType) => argumentsTypes.map(argType => noOccurrence(tvar, argType)).foldLeft(true)(_ && _) && noOccurrence(tvar, resultType)
      case VarType(_) => !(tvar == ty)
    }
  }

  def typeOf(exp: Expression, environment: Map[Expression, Type] = Map(), subst: Map[Type, VarType] = Map()): Answer = {
    exp match {
      case Const(_) => Answer(new IntType, subst)
      case Bool(_) => Answer(new BoolType, subst)
      case ref@Val(_) => environment.get(ref) match {
        case Some(value) => Answer(value, subst)
        case None => Answer(freshTvarType.getType(), subst)
      }
      case BinaryOperation(l, operation, r) => evaluateBinaryOperation(l, operation, r, subst, environment)
      case If(condition, ifThen, elseIf) => {
        val ans1 = typeOf(condition, environment, subst)
        unifier(ans1.ty, new BoolType, subst, condition)

        val ans2 = typeOf(ifThen, environment, subst)
        val ans3 = typeOf(elseIf, environment, subst)

        unifier(ans2.ty, ans3.ty, subst, exp)

        Answer(ans2.ty, subst)
      }
      case Lambda(arguments, body, returnType) => {
        val retType = `oType->type`(returnType)

        typeOf(body, environment ++ arguments.map { case (k, v) => (k, `oType->type`(v)) }, subst)

        new Answer(FunctionType(arguments.map(_._2).toList, retType), subst)
      }
      case Apply(lambda, parameters) => ???
      case ValDecl(variable, body, oVarTypes, oReturnType) => {
        val variableTypes = variable.map { case (k, _) => k -> `oType->type`(oVarTypes.get(k).getOrElse(null)) }
        val resultType = `oType->type`(oReturnType)

        val f = new FunctionType(variableTypes.values.toList, resultType)
        val ans = typeOf(body, environment ++ variableTypes, subst)

        unifier(ans.ty, resultType, subst, body);

        typeOf(body, environment ++ variable.map { case (k: Val, v: Expression) => (k, typeOf(v, environment, subst).ty) }, subst)
      }
    }
  }


  private def evaluateBinaryOperation(l: Expression, operation: Operation, r: Expression, subst: Map[Type, VarType], environment: Map[Expression, Type]) = {
    val ans1 = typeOf(l, environment, subst)
    unifier(ans1.ty, new IntType, ans1.subst, l)

    val ans2 = typeOf(r, environment, ans1.subst)
    unifier(ans2.ty, new IntType, ans2.subst, r)

    operation match {
      case _: IntOperation => Answer(new IntType, ans2.subst)
      case _: BoolOperation => Answer(new BoolType, ans2.subst)
    }
  }

  def `oType->type`(otype: Type) = {
    otype match {
      case AType(ty) => ty
      case null | NoType() => freshTvarType.getType()
      case _ => otype
    }
  }

  def typeToExternalForm(ty: Type): String = {
    ty match {
      case IntType() => "int"
      case BoolType() => "bool"
      case FunctionType(argumentsTypes, resultType) => argumentsTypes.map(argType => typeToExternalForm(argType)).mkString(", ") + " -> " + typeToExternalForm(resultType)
      case VarType(sn) => sn
    }
  }

  object freshTvarType {
    val counter = 0

    def getType(): VarType = {
      new VarType(s"t${counter + counter + 1}")
    }
  }

  case class Answer(ty: Type, subst: Map[Type, VarType] = Map())

}
