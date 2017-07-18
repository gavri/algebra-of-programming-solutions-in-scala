sealed abstract class Whole {
  def fold[T](c: T)(h: T => T) : T
  def f = fold(NaturalOne: Natural) {(x: Natural) => NaturalSucc(x)}
  def id = f.g
}

case object WholeZero extends Whole {
  def fold[T](c: T)(_h: T => T) = c
}

case class WholeSucc(w: Whole) extends Whole {
  def fold[T](c: T)(h: T => T) = h(w.fold(c)(h))
}

sealed abstract class Natural {
  def fold[T](c: T)(h: T => T) : T
  def g = fold(WholeZero: Whole) {(x: Whole) => WholeSucc(x)}
  def id = g.f
}

case object NaturalOne extends Natural {
  def fold[T](c: T)(_h: T => T) = c
}

case class NaturalSucc(n: Natural) extends Natural {
  def fold[T](c: T)(h: T => T) = h(n.fold(c)(h))
}
