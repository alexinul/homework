// all the available constructs offered by the language
sealed trait Expression

sealed trait Value extends Expression

final case class Const(value: Int) extends Value {
  override def toString: String = value.toString
}

final case class Val(value: String) extends Value {
  override def toString: String = value
}

final case class Eq(lhs: Expression, rhs: Expression) extends Expression

final case class If(condition: Expression, ifThen: Expression, elseIf: Expression) extends Expression

final case class  Lambda(arguments: List[String], body: Expression) extends Expression

final case class Apply(expression: Lambda, optExpression: Expression*) extends Expression

final case class ValDecl(value: Map[String, Expression], body: Expression) extends Expression

final case class BinaryOperation(operation: Operation, lhs: Expression, rhs: Expression) extends Expression

// the 4 types of available operations
sealed trait Operation {
  def apply(lhs: Int, rhs: Int): Const
}

final case object Add extends Operation {
  def apply(lhs: Int, rhs: Int): Const = Const(lhs + rhs)
}

final case object Sub extends Operation {
  def apply(lhs: Int, rhs: Int): Const = Const(lhs - rhs)
}

final case object Mul extends Operation {
  def apply(lhs: Int, rhs: Int): Const = Const(lhs * rhs)
}

final case object Div extends Operation {
  def apply(lhs: Int, rhs: Int): Const = Const(lhs / rhs)
}

// helper object for creating object
object Expression {
  def apply(i: Int): Const = Const(i)

  def apply(s: String): Val = Val(s)

  def apply(i: Expression, t: Expression, e: Expression): If = If(i, t, e)

  def apply(arguments: List[String], body: Expression): Lambda = new Lambda(arguments, body)

  def apply(expression: Lambda, optExpression: Expression*): Apply = new Apply(expression, optExpression: _*)

  def apply(value: Map[String, Expression], body: Expression): ValDecl = new ValDecl(value, body)

  def apply(o: Operation, l: Expression, r: Expression): BinaryOperation = new BinaryOperation(o, l, r)
}



