package example

/**
  * @author leopold
  * @since 17/09/18
  */
sealed trait Value[A] {

  def get: A = this match {
    case Val(a) => a
    case Calc(b, f) => f(b)
  }
}

case class Val[A](a: A) extends Value[A]
case class Calc[B, A](b: B, f: B => A) extends Value[A]
