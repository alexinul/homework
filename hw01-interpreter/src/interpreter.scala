object interpreter {

  def apply(program: Expression, environment: Map[String, Expression] = Map()): Either[Value, Lambda] =
    program match {
      case terminal@Const(_) => Left(terminal)
      case terminal@Val(_) => Left(terminal)
      case Eq(lhs, rhs) => evaluateEq(lhs, rhs, environment)
      case BinaryOperation(op, lhs, rhs) => evaluateBinaryOperation(op, lhs, rhs, environment)
      case If(condition, ifThen, elseIf) =>
        interpreter.apply(condition, environment) match {
          case Left(Const(1)) => interpreter.apply(ifThen, environment)
          case _ => interpreter.apply(elseIf, environment)
        }
      case Lambda(arguments, body) => Right(Lambda(arguments, body))
      case Apply(expression, parameters) =>
        interpreter.apply(expression, environment) match {
          case Left(value) => value match {
            case Const(_) => throw new RuntimeException("Cannot invoke Const")
            case Val(id) => evaluate(id, environment) match {
              case Left(_) => throw new RuntimeException("Cannot invoke Val")
              case Right(Lambda(_, body)) => interpreter.apply(body, environment ++ evaluateParameters(parameters, environment))
              case _ => throw new RuntimeException("Something unexpected happened")
            }
          }
          case Right(Lambda(_, body)) => interpreter.apply(body, environment ++ evaluateParameters(parameters, environment))
        }
      case ValDecl(value, body)
      => interpreter.apply(body, environment ++ value)
    }

  private def evaluate(id: Any, environment: Map[String, Expression]): Either[Value, Expression] = {
    environment.get(id.toString) match {
      case Some(value) => value match {
        case terminal@Const(_) => Left(terminal)
        case terminal@Lambda(_, _) => Right(terminal)
        case _ => throw new RuntimeException("Not going to get here")
      }
      case None => Left(Val(id.toString))
    }
  }

  private def evaluateEq(lhs: Expression, rhs: Expression, environment: Map[String, Expression]) = {
    interpreter.apply(lhs, environment) match {
      case Left(left) =>
        left match {
          case (Const(_) | Val(_)) =>
            interpreter.apply(rhs, environment) match {
              case Left(right) =>
                right match {
                  case (Const(_) | Val(_)) =>
                    Left(Const(if (evaluate(left, environment).merge.toString == evaluate(right, environment).merge.toString) 1 else 0))
                }
              case Right(_) => throw new RuntimeException("Cannot test equality between lambdas")
            }
        }
      case Right(_) => throw new RuntimeException("Cannot test equality between lambdas")
    }
  }

  private def evaluateBinaryOperation(op: Operation, lhs: Expression, rhs: Expression, environment: Map[String, Expression]) = {
    interpreter.apply(lhs, environment) match {
      case Left(value) =>
        value match {
          case Const(int1) =>
            interpreter.apply(rhs, environment) match {
              case Left(value) =>
                value match {
                  case Const(int2) => Left(op.apply(int1, int2))
                  case Val(id) => evaluate(id, environment) match {
                    case Left(value) => value match {
                      case Const(int2) => Left(op.apply(int1, int2))
                      case Val(_) => throw new RuntimeException("Cannot do binary operations between strings")
                    }
                    case Right(_) => throw new RuntimeException("Cannot do binary operations between lambdas")
                  }
                }
              case Right(_) => throw new RuntimeException("Cannot do binary operations between lambdas")
            }
          case Val(id) => evaluate(id, environment) match {
            case Left(value) => value match {
              case Const(int1) => interpreter.apply(rhs, environment) match {
                case Left(value) =>
                  value match {
                    case Const(int2) => Left(op.apply(int1, int2))
                    case Val(id) => evaluate(id, environment) match {
                      case Left(value) => value match {
                        case Const(int2) => Left(op.apply(int1, int2))
                        case Val(_) => throw new RuntimeException("Cannot do binary operations between strings")
                      }
                      case Right(_) => throw new RuntimeException("Cannot do binary operations between lambdas")
                    }
                  }
                case Right(_) => throw new RuntimeException("Cannot do binary operations between lambdas")
              }
              case Val(_) => throw new RuntimeException("Cannot do binary operations between strings")
            }
            case Right(_) => throw new RuntimeException("Cannot do binary operations between lambdas")
          }
        }
      case Right(_) => throw new RuntimeException("Cannot do binary operations between lambdas")
    }
  }

  private def evaluateParameters(parameters: Map[String, Expression], environment: Map[String, Expression]) =
    parameters.map { case (id, body) => id -> (interpreter.apply(body, environment) match {
      case terminal@(Left(_) | Right(_)) => terminal.merge
    })
    }
}
