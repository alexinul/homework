sealed trait Expression

sealed trait Value extends Expression

final case class Const(value: Int) extends Value {
  override def toString: String = value.toString
}

final case class Val(value: String) extends Value {
  override def toString: String = value
}

final case class Eq(l: Expression, r: Expression) extends Expression

final case class If(condition: Expression, ifThen: Expression, elseIf: Expression) extends Expression

final case class Lambda(arguments: List[String], body: Expression) extends Expression

final case class Apply(lambda: Expression, parameters: Map[String, Expression]) extends Expression

final case class ValDecl(variable: Map[String, Expression], body: Expression) extends Expression

final case class BinaryOperation(operation: Operation, l: Expression, r: Expression) extends Expression

sealed trait Operation {
  def apply(l: Int, r: Int): Const
}

final case object Add extends Operation {
  def apply(l: Int, r: Int): Const = Const(l + r)
}

final case object Sub extends Operation {
  def apply(l: Int, r: Int): Const = Const(l - r)
}

final case object Mul extends Operation {
  def apply(l: Int, r: Int): Const = Const(l * r)
}

final case object Div extends Operation {
  def apply(l: Int, r: Int): Const = Const(l / r)
}

object Expression {
  def apply(i: Int): Const = Const(i)

  def apply(s: String): Val = Val(s)

  def apply(l: Expression, r: Expression): Eq = new Eq(l, r)

  def apply(i: Expression, t: Expression, e: Expression): If = If(i, t, e)

  def apply(arguments: List[String], body: Expression): Lambda = new Lambda(arguments, body)

  def apply(expression: Expression, parameters: Map[String, Expression]): Apply = new Apply(expression, parameters)

  def apply(value: Map[String, Expression], body: Expression): ValDecl = new ValDecl(value, body)

  def apply(o: Operation, l: Expression, r: Expression): BinaryOperation = new BinaryOperation(o, l, r)
}



