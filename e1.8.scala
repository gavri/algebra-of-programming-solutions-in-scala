sealed abstract class ListR[T] {
  def fold[A](c: A)(h: (A, T) => A): A

  private def h(listL: ListL[T], x: T): ListL[T] = listL match {
    case ListLNull() => Snoc(ListLNull(), x)
    case Snoc(left, a) => Snoc((h(left, x)), a)
  }

  def convert: ListL[T] = {
    val c: ListL[T] = ListLNull()
    fold(c)(h)
  }

  def catConv(other: ListL[T]): ListL[T] = fold(other)(h)

}
case class ListRNull[T]() extends ListR[T] {
  def fold[A](c: A)(_h: (A, T) => A): A = c
}
case class Cons[T](a: T, right: ListR[T]) extends ListR[T] {
  def fold[A](c: A)(h: (A, T) => A): A = h(right.fold(c)(h), a)
}

sealed abstract class ListL[T]
case class ListLNull[T]() extends ListL[T]
case class Snoc[T](left: ListL[T], a: T) extends ListL[T]
