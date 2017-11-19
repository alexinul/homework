object interpreter {

  def apply(program: Expression, environment: Map[String, Expression] = Map()): Either[Value, Lambda] =
    program match {
      case terminal@Const(_) => Left(terminal)
      case terminal@Val(_) => Left(terminal)
      case Lambda(arguments, body) => Right(Lambda(arguments, body))
      case Eq(lhs, rhs) => evaluateEq(lhs, rhs, environment)
      case BinaryOperation(op, lhs, rhs) => evaluateBinaryOperation(op, lhs, rhs, environment)
      case If(condition, ifThen, elseIf) => evaluateIf(environment, condition, ifThen, elseIf)
      case Apply(expression, parameters) =>
        interpreter.apply(expression, environment) match {
          case Left(value) => value match {
            case Const(_) => throw new RuntimeException("Cannot invoke Const")
            case Val(id) => evaluate(id, environment) match {
              case Left(_) => throw new RuntimeException("Cannot invoke Val")
              case Right(Lambda(_, body)) => interpreter.apply(body, environment ++ evaluateParameters(parameters, environment))
              case _ => throw new RuntimeException("Error during function invocation")
            }
          }
          case Right(Lambda(_, body)) => interpreter.apply(body, environment ++ evaluateParameters(parameters, environment))
        }
      case ValDecl(value, body) => interpreter.apply(body, environment ++ value)
    }

  private def evaluateIf(environment: Map[String, Expression], condition: Expression, ifThen: Expression, elseIf: Expression) = {
    interpreter.apply(condition, environment) match {
      case Left(Const(1)) => evaluateBOPart(ifThen, environment)
      case _ => evaluateBOPart(elseIf, environment)
    }
  }

  private def evaluateEq(lhs: Expression, rhs: Expression, environment: Map[String, Expression]) = {
    val x = evaluateBOPart(lhs, environment)
    val y = evaluateBOPart(rhs, environment)

    (x, y) match {
      case (Left(Const(l)), Left(Const(r))) => Left(Const(if (l == r) 1 else 0))
      case _ => throw new RuntimeException("An error occurred during equality check evaluation")
    }
  }

  private def evaluateBinaryOperation(op: Operation, lhs: Expression, rhs: Expression, environment: Map[String, Expression]) = {
    val x = evaluateBOPart(lhs, environment)
    val y = evaluateBOPart(rhs, environment)

    (x, y) match {
      case (Left(Const(l)), Left(Const(r))) => Left(op.apply(l, r))
      case _ => throw new RuntimeException("An error occurred during binary operation evaluation")
    }
  }

  private def evaluateBOPart(expression: Expression, environment: Map[String, Expression]) = {
    interpreter.apply(expression, environment) match {
      case Left(value) =>
        value match {
          case terminal@Const(_) => Left(terminal)
          case Val(id) => evaluate(id, environment) match {
            case Left(value) => value match {
              case terminal@Const(_) => Left(terminal)
              case _ => throw new RuntimeException("An error occurred during binary operation evaluation")
            }
            case Right(exp) => interpreter.apply(exp, environment)
          }
        }
      case Right(_) => throw new RuntimeException("Cannot do binary operations between lambdas")
    }
  }

  private def evaluate(id: Any, environment: Map[String, Expression]): Either[Value, Expression] = {
    environment.get(id.toString) match {
      case Some(value) => value match {
        case terminal@Const(_) => Left(terminal)
        case nonTerminal@_ => Right(nonTerminal)
      }
      case None => throw new RuntimeException("No such element has been defined")
    }
  }

  private def evaluateParameters(parameters: Map[String, Expression], environment: Map[String, Expression]) =
    parameters.map { case (id, body) => id -> (interpreter.apply(body, environment) match {
      case terminal@(Left(_) | Right(_)) => terminal.merge
    })
    }
}
