sealed abstract class Nat {
  def fold[T](c: T)(h: T => T) : T
  def last(p: Nat => Boolean) : Nat
}

case object Zero extends Nat {
  def fold[T](c: T)(_h: T => T) = c
  def last(_p: Nat => Boolean) = Zero
}

case class Succ(n: Nat) extends Nat {
  def fold[T](c: T)(h: T => T) = h(n.fold(c)(h))
  def last(p: Nat => Boolean) = {
    val f = (x: (Nat, Nat)) => x._2
    val c: (Nat, Nat) = (Zero, Zero)
    val h = (pair: (Nat, Nat)) => {
      val (previous, answer) = pair
      val current = Succ(previous)
      if (p(current))
        (current, current)
      else
        (current, answer)
    }
    f(fold(c)(h))
  }
}

object NatPredicates {
  def isEven(n: Nat): Boolean = n match {
    case Zero => true
    case Succ(Zero) => false
    case Succ(Succ(n)) => isEven(n)
  }
}
