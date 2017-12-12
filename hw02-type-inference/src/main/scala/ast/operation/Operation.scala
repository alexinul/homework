package ast.operation

import ast.Value

trait Operation {
  def apply(l: Int, r: Int): Value
}

