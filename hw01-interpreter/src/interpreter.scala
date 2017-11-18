object interpreter {
  def apply(program: Expression): Either[Value, Lambda] =
    program match {
      case terminal@Const(_) => Left(terminal)
      case terminal@Val(_) => Left(terminal)
      case Eq(lhs, rhs) => evaluateEq(lhs, rhs)
      case BinaryOperation(op, lhs, rhs) => evaluateBinaryOperation(op, lhs, rhs)
      case If(condition, ifThen, elseIf)
      => interpreter.apply(condition) match {
        case Left(Const(1)) => interpreter.apply(ifThen)
        case _ => interpreter.apply(elseIf)
      }
      case Lambda(arguments, body)
      => ???
      case Apply(expression, optExpression)
      => ???
      case ValDecl(value, body)
      =>
        ???
    }

  private def evaluateEq(lhs: Expression, rhs: Expression) = {
    interpreter.apply(lhs) match {
      case Left(value) =>
        value match {
          case lRes@(Const(_) | Val(_)) =>
            interpreter.apply(rhs) match {
              case Left(value) =>
                value match {
                  case rRes@(Const(_) | Val(_)) =>
                    Left(Const(if (lRes.toString == rRes.toString) 1 else 0))
                }
              case Right(_) => throw new RuntimeException("Cannot test equality between lambdas")
            }
        }
      case Right(_) => throw new RuntimeException("Cannot test equality between lambdas")
    }
  }

  private def evaluateBinaryOperation(op: Operation, lhs: Expression, rhs: Expression) = {
    interpreter.apply(lhs) match {
      case Left(value) =>
        value match {
          case Const(int1) =>
            interpreter.apply(rhs) match {
              case Left(value) =>
                value match {
                  case Const(int2) => Left(op.apply(int1, int2))
                  case Val(_) => throw new RuntimeException("Cannot do binary operations between strings")
                }
              case Right(value) => throw new RuntimeException("Cannot do binary operations between lambdas")
            }
          case Val(_) => throw new RuntimeException("Cannot do binary operations between strings")
        }
      case Right(_) => throw new RuntimeException("Cannot do binary operations between lambdas")
    }
  }
}