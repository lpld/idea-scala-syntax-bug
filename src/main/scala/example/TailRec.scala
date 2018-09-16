package example

import scala.annotation.tailrec

trait TailRec[A] { self =>

  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
}

object TailRec {

  @tailrec def run[A](tr: TailRec[A]): A = tr match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A]
case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]


