sealed abstract class Nat {
  def fold[T](c: T)(h: T => T) : T
  private def +(other: Nat) = fold(other){Succ(_)}
  private def *(other: Nat) = fold(Zero: Nat){this + _}
  def square = this * this
}

case object Zero extends Nat {
  def fold[T](c: T)(_h: T => T) = c
}

case class Succ(n: Nat) extends Nat {
  def fold[T](c: T)(h: T => T) = h(n.fold(c)(h))
}
