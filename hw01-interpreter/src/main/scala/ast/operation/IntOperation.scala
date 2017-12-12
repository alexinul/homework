package ast.operation

import ast.Const

sealed trait IntOperation extends Operation

final case object Add extends IntOperation {
  def apply(l: Int, r: Int) = Const(l + r)
}

final case object Sub extends IntOperation {
  def apply(l: Int, r: Int) = Const(l - r)
}

final case object Mul extends IntOperation {
  def apply(l: Int, r: Int) = Const(l * r)
}

final case object Div extends IntOperation {
  def apply(l: Int, r: Int) = Const(l / r)
}
