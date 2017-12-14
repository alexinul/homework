import ast.{Type, _}
import ast.operation._

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
      case If(condition, ifThen, elseIf) => {
        val ans1 = typeOf(condition, environment, subst)
        val subst1 = unifier(ans1.ty, new BoolType, ans1.subst, condition)

        val ans2 = typeOf(ifThen, environment, subst1)
        val ans3 = typeOf(elseIf, environment, ans2.subst)

        val subst2 = unifier(ans2.ty, ans3.ty, ans3.subst, exp)

        Answer(ans2.ty, subst2)
      }
      case Lambda(arguments, body) => evaluateLambda(environment, subst, arguments, body)
      case Apply(lambda, parameters) => {
        val resultType = freshTvarType.getType()
        val ans = typeOf(lambda, environment, subst)
        val pAns = parameters.map { case (v, e) => v -> typeOf(e, environment, subst).ty }

        unifier(ans.ty, FunctionType(pAns.values.toList, resultType), subst, exp)

        new Answer(resultType, subst)
      }
      case ValDecl(variable, body, oVarTypes, oReturnType) => {
        typeOf(body, environment ++ variable.map { case (v, e) => v -> typeOf(e, environment, subst).ty }, subst)

        //        val variableTypes = variable.map { case (k, _) => k -> `oType->type`(oVarTypes.get(k).getOrElse(null)) }
        //        val resultType = `oType->type`(oReturnType)
        //
        //        val f = new FunctionType(variableTypes.values.toList, resultType)
        //        val ans = typeOf(body, environment ++ variableTypes, subst)
        //
        //        unifier(ans.ty, resultType, subst, body);
        //
        //        typeOf(body, environment ++ variable.map { case (k: Val, v: Expression) => (k, typeOf(v, environment, subst).ty) }, subst)
      }
    }
  }

  def applyOneSubst(ty0: Type, tvar: VarType, ty1: Type): Type = {
    ty0 match {
      case self@(IntType() | BoolType()) => self
      case FunctionType(argumentsTypes, resultType) => FunctionType(argumentsTypes.map(argType => applyOneSubst(argType, tvar, ty1)), applyOneSubst(resultType, tvar, ty1))
      case VarType(_) => if (ty0 != tvar) ty1 else ty0
    }
  }

  def applySubstToType(ty: Type, subst: Map[VarType, Type]): Type = {
    ty match {
      case self@(IntType() | BoolType()) => self
      case FunctionType(argumentsTypes, resultType) => FunctionType(argumentsTypes.map(argType => applySubstToType(argType, subst)), applySubstToType(resultType, subst))
      case self@VarType(_) => subst.get(self) match {
        case Some(value) => value
        case None => ty
      }
    }
  }

  def extendSubst(subst: Map[VarType, Type], tvar: VarType, ty: Type): Map[VarType, Type] = if (subst.size == 0) subst ++ Map(tvar -> ty)
  else subst.map { case (k, v) => k -> applyOneSubst(v, tvar, ty) } ++ Map(tvar -> ty)

  def unifier(ty1: Type, ty2: Type, subst: Map[VarType, Type], exp: Expression): Map[VarType, Type] = {
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
          (argTy1 zip argTy2).map { case (arg1, arg2) => subst ++ unifier(arg1, arg2, subst, exp) }
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

  private def evaluateLambda(environment: Map[Expression, Type], subst: Map[VarType, Type], arguments: List[Val], body: Expression) = {
    val args = arguments.map { v => v -> `oType->type`(new NoType) }.toMap
    val ans = typeOf(body, environment ++ args, subst)

    new Answer(FunctionType(args.values.toList, ans.ty), ans.subst)
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

  def `oType->type`(otype: Type) = {
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
      case FunctionType(argumentsTypes, resultType) => argumentsTypes.map(argType => typeToExternalForm(argType, subst)).mkString(", ") + " -> " + typeToExternalForm(resultType, subst)
      case self@VarType(_) => subst.get(self) match {
        case Some(value) => typeToExternalForm(value)
        case None => self.sn
      }
    }
  }

  object freshTvarType {
    var counter = 0

    def getType(): VarType = {
      counter += 1
      new VarType(s"t${counter}")
    }
  }

  case class Answer(ty: Type, subst: Map[VarType, Type] = Map())

}
