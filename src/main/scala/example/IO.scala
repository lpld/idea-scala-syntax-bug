package example

import scala.annotation.tailrec

trait IO[A] { self =>

  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}

object IO {

  @tailrec def run[A](tr: IO[A]): A = tr match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[B, A](sub: IO[B], k: B => IO[A]) extends IO[A]


