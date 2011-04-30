package ad

case class Forward[A](primal: A, tangent: A) 

object Forward { 
  implicit def ForwardMode[A] : Mode[Forward, A] = new Mode[Forward, A] {
    def compare(a: Forward[A], b: Forward[A])(implicit A: Numeric[A]) = A.compare(a.primal, b.primal)

    def plus(a: Forward[A], b: Forward[A])(implicit A: Numeric[A]) = 
      Forward[A](A.plus(a.primal,b.primal), A.plus(a.tangent,b.tangent))

    def minus(a: Forward[A], b: Forward[A])(implicit A: Numeric[A]) = 
      Forward[A](A.minus(a.primal,b.primal), A.minus(a.tangent, b.tangent))

    def times(a: Forward[A], b: Forward[A])(implicit A: Numeric[A]) = 
      Forward[A](A.times(a.primal,b.primal), A.plus(A.times(a.primal,b.tangent), A.times(a.tangent,b.primal)))

    def lift(a: A)(implicit A: Numeric[A]) = Forward[A](a, A.zero)

    def recip(a: Forward[A])(implicit A: Fractional[A]) = { 
      val ooa = A.div(A.one, a.primal)
      Forward[A](ooa, A.negate(A.times(ooa, ooa)))
    }

    def negate(a: Forward[A])(implicit A: Numeric[A]) =
      Forward[A](A.negate(a.primal), A.negate(a.tangent))

    def todo = error("todo")

    def abs(a: Forward[A])(implicit A: Numeric[A]) = {
      if (A.compare(a.primal,A.zero) == -1) negate(a)
      else a
    }

    def signum(a: Forward[A])(implicit A: Numeric[A]) = A.signum(a.primal)
    def toLong(a: Forward[A])(implicit A: Numeric[A]) = A.toLong(a.primal)
    def toInt(a: Forward[A])(implicit A: Numeric[A]) = A.toInt(a.primal)

    def exp(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def log(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo

    def sin(a: Forward[A])(implicit A: Floating[A]) = Forward(A.sin(a.primal), A.cos(a.tangent))
    def cos(a: Forward[A])(implicit A: Floating[A]) = Forward(A.cos(a.primal), A.negate(A.sin(a.tangent)))
    override def tan(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo

    def sinh(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def cosh(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    override def tanh(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo

    def asin(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def acos(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
    def atan(a: Forward[A])(implicit A: Floating[A]): Forward[A] = todo
  }
}
