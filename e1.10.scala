sealed abstract class ListR[T] {
  def fold[A](c: A)(h: (T, A) => A): A
  def cat(other: ListR[T]) = fold(other)(Cons(_, _))

}
case class ListRNull[T]() extends ListR[T] {
  def fold[A](c: A)(_h: (T, A) => A): A = c
}
case class Cons[T](a: T, right: ListR[T]) extends ListR[T] {
  def fold[A](c: A)(h: (T, A) => A): A = h(a, right.fold(c)(h))
}
