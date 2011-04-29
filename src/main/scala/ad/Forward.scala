package ad

case class Forward[A](primal: A, tangent: A) 

object Forward { 
  implicit object ForwardMode extends Mode[Forward] {
    def compare[A](a: Forward[A], b: Forward[A])(implicit A: Numeric[A]) = A.compare(a.primal, b.primal)

    def plus[A](a: Forward[A], b: Forward[A])(implicit A: Numeric[A]) = 
      Forward[A](A.plus(a.primal,b.primal), A.plus(a.tangent,b.tangent))

    def minus[A](a: Forward[A], b: Forward[A])(implicit A: Numeric[A]) = 
      Forward[A](A.minus(a.primal,b.primal), A.minus(a.tangent, b.tangent))

    def times[A](a: Forward[A], b: Forward[A])(implicit A: Numeric[A]) = 
      Forward[A](A.times(a.primal,b.primal), A.plus(A.times(a.primal,b.tangent), A.times(a.tangent,b.primal)))

    def lift[A](a: A)(implicit A: Numeric[A]) = Forward[A](a, A.zero)

    def recip[A](a: Forward[A])(implicit A: Fractional[A]) = { 
      val ooa = A.div(A.one, a.primal)
      Forward[A](ooa, A.negate(A.times(ooa, ooa)))
    }

    def negate[A](a: Forward[A])(implicit A: Numeric[A]) =
      Forward[A](A.negate(a.primal), A.negate(a.tangent))

    def todo = error("todo")

    def abs[A](a: Forward[A])(implicit A: Numeric[A]) = {
      if (A.compare(a.primal,A.zero) == -1) negate(a)
      else a
    }

    def signum[A](a: Forward[A])(implicit A: Numeric[A]) = A.signum(a.primal)
    def toLong[A](a: Forward[A])(implicit A: Numeric[A]) = A.toLong(a.primal)
    def toInt[A](a: Forward[A])(implicit A: Numeric[A]) = A.toInt(a.primal)

    def exp[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def log[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo

    def sin[A](a: Forward[A])(implicit A: Floating[A]) = Forward(A.sin(a.primal), A.cos(a.tangent))
    def cos[A](a: Forward[A])(implicit A: Floating[A]) = Forward(A.cos(a.primal), A.negate(A.sin(a.tangent)))
    def tan[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo

    def sinh[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def cosh[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def tanh[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo

    def asin[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def acos[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def atan[A](a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
  }
}
