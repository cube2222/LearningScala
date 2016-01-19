package LearningScala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree [A]
case class Branch[A](left: Tree[A],right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
      fold(t)(x => 1)(_ + _ + 1)
  }
  def maximum(t: Tree[Int]): Int = {
      fold(t)(x => x)((x,y) => x max y)
  }
  def depth[A](t: Tree[A]): Int = {
      fold(t)(_ => 0)((x,y) => (x max y) + 1)
  }
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
  }
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Branch(Leaf(x), Leaf(y)) => g(f(x),f(y))
    case Branch(xt, Leaf(y)) => g(fold(xt)(f)(g), f(y))
    case Branch(Leaf(x), yt) => g(f(x), fold(yt)(f)(g))
  }
}