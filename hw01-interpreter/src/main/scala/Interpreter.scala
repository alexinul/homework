import ast._
import ast.operation.Operation

object Interpreter {

  def apply(program: Expression, environment: Map[String, Expression] = Map()): Expression =
    program match {
      case terminal@(Const(_)|Val(_)|Bool(_)|Lambda(_,_)) => terminal
      case BinaryOperation(lhs, op, rhs) => evaluateBinaryOperation(lhs, op, rhs, environment)
      case If(condition, ifThen, elseIf) => evaluateIf(condition, ifThen, elseIf, environment)
      case Apply(expression, parameters) =>
        Interpreter.apply(expression, environment) match {
          case Val(id) => evaluate(id, environment) match {
            case Lambda(_, body) => Interpreter.apply(body, environment ++ evaluateParameters(parameters, environment))
            case _ => throw new RuntimeException("Error during function invocation")
          }
          case Lambda(_, body) => Interpreter.apply(body, environment ++ evaluateParameters(parameters, environment))
          case _ => throw new RuntimeException("Error during function invocation")
        }
      case ValDecl(value, body)
      => Interpreter.apply(body, environment ++ value)
    }

  private def evaluateIf(condition: Expression, ifThen: Expression, elseIf: Expression, environment: Map[String, Expression]) = {
    Interpreter.apply(condition, environment) match {
      case Bool(true) => evaluateBOPart(ifThen, environment)
      case Bool(false) => evaluateBOPart(elseIf, environment)
      case _ => throw new RuntimeException("The if condition is not a boolean opeartion")
    }
  }

  private def evaluateBinaryOperation(lhs: Expression, op: Operation, rhs: Expression, environment: Map[String, Expression]) = {
    val x = evaluateBOPart(lhs, environment)
    val y = evaluateBOPart(rhs, environment)

    (x, y) match {
      case (Const(l), Const(r)) => op.apply(l, r)
      case _ => throw new RuntimeException("An error occurred during binary operation evaluation")
    }
  }

  private def evaluateBOPart(expression: Expression, environment: Map[String, Expression]) = {
    Interpreter.apply(expression, environment) match {
      case terminal@(Const(_)|Bool(_)) => terminal
      case Val(id) => evaluate(id, environment) match {
        case terminal@(Const(_)|Bool(_)) => terminal
        case nonTerminal@_ => Interpreter.apply(nonTerminal, environment)
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
    parameters.map { case (id, body) => id -> (Interpreter.apply(body, environment)) }
}
