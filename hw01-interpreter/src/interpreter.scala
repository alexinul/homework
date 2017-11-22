object interpreter {

  def apply(program: Expression, environment: Map[String, Expression] = Map()): Expression =
    program match {
      case terminal@Const(_) => terminal
      case terminal@Val(_) => terminal
      case terminal@Lambda(_, _) => terminal
      case Eq(lhs, rhs) => evaluateEq(lhs, rhs, environment)
      case BinaryOperation(op, lhs, rhs) => evaluateBinaryOperation(op, lhs, rhs, environment)
      case If(condition, ifThen, elseIf) => evaluateIf(condition, ifThen, elseIf, environment)
      case Apply(expression, parameters) =>
        interpreter.apply(expression, environment) match {
          case Val(id) => evaluate(id, environment) match {
            case Lambda(_, body) => interpreter.apply(body, environment ++ evaluateParameters(parameters, environment))
            case _ => throw new RuntimeException("Error during function invocation")
          }
          case Lambda(_, body) => interpreter.apply(body, environment ++ evaluateParameters(parameters, environment))
          case _ => throw new RuntimeException("Error during function invocation")
        }
      case ValDecl(value, body)
      => interpreter.apply(body, environment ++ value)
    }

  private def evaluateIf(condition: Expression, ifThen: Expression, elseIf: Expression, environment: Map[String, Expression]) = {
    interpreter.apply(condition, environment) match {
      case Const(1) => evaluateBOPart(ifThen, environment)
      case _ => evaluateBOPart(elseIf, environment)
    }
  }

  private def evaluateEq(lhs: Expression, rhs: Expression, environment: Map[String, Expression]) = {
    val x = evaluateBOPart(lhs, environment)
    val y = evaluateBOPart(rhs, environment)

    (x, y) match {
      case (Const(l), Const(r)) => Const(if (l == r) 1 else 0)
      case _ => throw new RuntimeException("An error occurred during equality check evaluation")
    }
  }

  private def evaluateBinaryOperation(op: Operation, lhs: Expression, rhs: Expression, environment: Map[String, Expression]) = {
    val x = evaluateBOPart(lhs, environment)
    val y = evaluateBOPart(rhs, environment)

    (x, y) match {
      case (Const(l), Const(r)) => op.apply(l, r)
      case _ => throw new RuntimeException("An error occurred during binary operation evaluation")
    }
  }

  private def evaluateBOPart(expression: Expression, environment: Map[String, Expression]) = {
    interpreter.apply(expression, environment) match {
      case terminal@Const(_) => terminal
      case Val(id) => evaluate(id, environment) match {
        case terminal@Const(_) => terminal
        case nonTerminal@_ => interpreter.apply(nonTerminal, environment)
      }
      case _ => throw new RuntimeException("Cannot do binary operations between lambdas")
    }
  }

  private def evaluate(id: Any, environment: Map[String, Expression]) = {
    environment.get(id.toString) match {
      case Some(value) => value
      case None => throw new RuntimeException("No such element has been defined")
    }
  }

  private def evaluateParameters(parameters: Map[String, Expression], environment: Map[String, Expression]) =
    parameters.map { case (id, body) => id -> (interpreter.apply(body, environment)) }
}
