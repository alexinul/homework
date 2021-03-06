import ast._
import ast.operation._

import scala.util.Random

object Checker {
  def typeOf(exp: Expression, environment: Map[Expression, Type] = Map(), subst: Map[VarType, Type] = Map()): Answer = {
    exp match {
      case Const(_) => Answer(new IntType, subst)
      case Bool(_) => Answer(new BoolType, subst)
      case ref@Val(_) => environment.get(ref) match {
        case Some(value) => Answer(value, subst)
        case None => throw new RuntimeException("Val not declared")
      }
      case BinaryOperation(l, operation, r) => evaluateBinaryOperation(l, operation, r, subst, environment)
      case If(condition, ifThen, elseIf) => evaluateIf(exp, environment, subst, condition, ifThen, elseIf)
      case Lambda(arguments, body, optReturnType) => evaluateLambda(environment, subst, arguments, body, optReturnType)
      case Apply(lambda, parameters) => evaluateApply(exp, environment, subst, lambda, parameters)
      case ValDecl(variable, body) => evaluateValDecl(environment, subst, variable, body)
    }
  }

  private def applyOneSubst(type1: Type, varType: VarType, type2: Type): Type = {
    type1 match {
      case self@(IntType() | BoolType()) => self
      case UnionType(ty1, ty2) => UnionType(applyOneSubst(ty1, varType, type2), applyOneSubst(ty2, varType, type2))
      case FunctionType(argumentsTypes, resultType) => FunctionType(argumentsTypes.map(argType => applyOneSubst(argType, varType, type2)), applyOneSubst(resultType, varType, type2))
      case VarType(_) => if (type1 != varType) type2 else type1
    }
  }

  private def applySubstToType(ty: Type, subst: Map[VarType, Type]): Type = {
    ty match {
      case self@(IntType() | BoolType()) => self
      case UnionType(ty1, ty2) => UnionType(applySubstToType(ty1, subst), applySubstToType(ty2, subst))
      case FunctionType(argumentsTypes, resultType) => FunctionType(argumentsTypes.map(argType => applySubstToType(argType, subst)), applySubstToType(resultType, subst))
      case self@VarType(_) => subst.get(self) match {
        case Some(value) => value
        case None => ty
      }
    }
  }

  private def extendSubst(subst: Map[VarType, Type], tvar: VarType, ty: Type): Map[VarType, Type] = if (subst.size == 0) subst ++ Map(tvar -> ty)
  else subst.map { case (k, v) => k -> applyOneSubst(v, tvar, ty) } ++ Map(tvar -> ty)

  private def unifier(ty1: Type, ty2: Type, subst: Map[VarType, Type], exp: Expression): Map[VarType, Type] = {
    val ty11 = applySubstToType(ty1, subst)
    val ty22 = applySubstToType(ty2, subst)

    if (ty11 == ty22) subst
    else
      (ty11, ty22) match {
        case (self@VarType(_), _) => if (noOccurrence(self, ty22)) extendSubst(subst, self, ty22) else throw new RuntimeException("Cannot find type")
        case (_, self@VarType(_)) => if (noOccurrence(self, ty11)) extendSubst(subst, self, ty11) else throw new RuntimeException("Cannot find type")
        case (FunctionType(argTy1, resTy1), FunctionType(argTy2, resTy2)) => (argTy1 zip argTy2).foldLeft(subst) { case (s, args) => s ++ unifier(args._1, args._2, s, exp) } ++ unifier(resTy1, resTy2, subst, exp)
        case (_, _) => throw new RuntimeException("Cannot unify type")
      }
  }

  private def noOccurrence(tvar: VarType, ty: Type): Boolean = {
    ty match {
      case IntType() => true
      case BoolType() => true
      case UnionType(ty1, ty2) => noOccurrence(tvar, ty1) && noOccurrence(tvar, ty2)
      case FunctionType(argumentsTypes, resultType) => argumentsTypes.map(argType => noOccurrence(tvar, argType)).foldLeft(true)(_ && _) && noOccurrence(tvar, resultType)
      case VarType(_) => !(tvar == ty)
    }
  }

  private def evaluateValDecl(environment: Map[Expression, Type], subst: Map[VarType, Type], variable: Map[Val, Expression], body: Expression) = {

    val resultType = freshTvarType.getType()
    val env = environment ++ variable.map { case (v, _) => v -> freshTvarType.getType() }
    val params = variable.map { case (_, e) => e -> typeOf(e, env, subst) }

    val subst1 = params.foldLeft(subst) { case (m, p) => m ++ unifier(p._2.ty, resultType, p._2.subst, p._1) }
    val ans = typeOf(body, env, subst1)

    Answer(resultType, ans.subst)
  }

  private def evaluateApply(exp: Expression, environment: Map[Expression, Type], subst: Map[VarType, Type], lambda: Expression, parameters: Map[Val, Expression]) = {

    val resultType = freshTvarType.getType()
    val ans = typeOf(lambda, environment, subst)
    val pAns = parameters.map { case (v, e) => v -> typeOf(e, environment, ans.subst) }

    val subst1 = unifier(ans.ty, FunctionType(pAns.values.map(a => a.ty).toList, resultType), ans.subst, exp)

    Answer(resultType, subst1)
  }

  private def evaluateLambda(environment: Map[Expression, Type], subst: Map[VarType, Type], arguments: List[Val], body: Expression, optReturnType: Type): Answer = {
    val args = arguments.map { v => v -> freshTvarType.getType() }.toMap
    val ans = typeOf(body, environment ++ args, subst)
    val ty = FunctionType(args.values.toList, ans.ty)

    if (!optReturnType.isInstanceOf[NoType]) {
      return Answer(ty, unifier(ans.ty, optReturnType, ans.subst, body))
    }
    Answer(ty, ans.subst)
  }

  private def evaluateIf(exp: Expression, environment: Map[Expression, Type], subst: Map[VarType, Type], condition: Expression, ifThen: Expression, elseIf: Expression) = {

    val ans1 = typeOf(condition, environment, subst)
    val subst1 = unifier(ans1.ty, new BoolType, ans1.subst, condition)

    val ans2 = typeOf(ifThen, environment, subst1)
    val ans3 = typeOf(elseIf, environment, ans2.subst)

    val ty1 = applySubstToType(ans2.ty, ans3.subst)
    val ty2 = applySubstToType(ans3.ty, ans3.subst)

    (ty1, ty2) match {
      case (VarType(_), _) => Answer(ans2.ty, unifier(ans2.ty, ans3.ty, ans3.subst, exp))
      case (_, VarType(_)) => Answer(ans2.ty, unifier(ans2.ty, ans3.ty, ans3.subst, exp))
      case (VarType(_), VarType(_)) => Answer(ans2.ty, unifier(ans2.ty, ans3.ty, ans3.subst, exp))
      case (_, _) => if (ty1 != ty2) Answer(new UnionType(ans2.ty, ans3.ty), ans3.subst) else Answer(ty2, ans3.subst)
    }
  }

  private def evaluateBinaryOperation(l: Expression, operation: Operation, r: Expression, subst: Map[VarType, Type], environment: Map[Expression, Type]) = {
    val ans1 = typeOf(l, environment, subst)
    val subst1 = unifier(ans1.ty, new IntType, ans1.subst, l)

    val ans2 = typeOf(r, environment, subst1)
    val subst2 = unifier(ans2.ty, new IntType, ans2.subst, r)

    operation match {
      case _: IntOperation => Answer(new IntType, subst2)
      case _: BoolOperation => Answer(new BoolType, subst2)
    }
  }

  private def `oType->type`(otype: Type) = {
    otype match {
      case AType(ty) => ty
      case null | NoType() => freshTvarType.getType()
      case _ => otype
    }
  }

  def typeToExternalForm(ty: Type, subst: Map[VarType, Type] = Map()): String = {
    ty match {
      case IntType() => "int"
      case BoolType() => "bool"
      case UnionType(ty1, ty2) => "[" + typeToExternalForm(ty1, subst) + "|" + typeToExternalForm(ty2, subst) + "]"
      case FunctionType(argumentsTypes, resultType) => "(" + (argumentsTypes.map(argType => typeToExternalForm(argType, subst)).mkString(", ")) + ")" + " -> " + typeToExternalForm(resultType, subst)
      case self@VarType(_) => subst.get(self) match {
        case Some(value) => typeToExternalForm(value, subst)
        case None => "t"
      }
    }
  }

  private object freshTvarType {

    def getType(): VarType = {
      new VarType(s"${Random.nextInt}")
    }
  }

  case class Answer(ty: Type, subst: Map[VarType, Type] = Map())

}
