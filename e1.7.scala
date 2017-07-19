sealed abstract class ListL[T] {
  def fold[A](c: A)(h: (A, T) => A): A
  def convert: ListR[T] = {
    val c: ListR[T] = ListRNull
    def h(listR: ListR[T], x: T): ListR[T] = listR match {
      case ListRNull => Cons(x, ListRNull)
      case Cons(a, right) => Cons(a, (h(right, x)))
    }
    fold(c)(h)
  }
}
case class ListLNull[T]() extends ListL[T] {
  def fold[A](c: A)(_h: (A, T) => A): A = c
}
case class Snoc[T](left: ListL[T], a: T) extends ListL[T] {
  def fold[A](c: A)(h: (A, T) => A): A = h(left.fold(c)(h), a)
}

sealed abstract class ListR[+T]
case object ListRNull extends ListR
case class Cons[T](a: T, right: ListR[T]) extends ListR[T]
