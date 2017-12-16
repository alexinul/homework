package unionTypes.ast.operation

import ast.Bool

sealed trait BoolOperation extends Operation

final case object Eq extends BoolOperation {
  def apply(l: Int, r: Int) = Bool(l == r)
}

final case object NEq extends BoolOperation {
  def apply(l: Int, r: Int) = Bool(l != r)
}

final case object GT extends BoolOperation {
  def apply(l: Int, r: Int) = Bool(l > r)
}

final case object GTE extends BoolOperation {
  def apply(l: Int, r: Int) = Bool(l >= r)
}

final case object LT extends BoolOperation {
  def apply(l: Int, r: Int) = Bool(l < r)
}

final case object LTE extends BoolOperation {
  def apply(l: Int, r: Int) = Bool(l <= r)
}