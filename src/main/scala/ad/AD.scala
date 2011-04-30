package ad

class AD[S[_], A](val guts: S[A])(implicit val mode: Mode[S, A]) { 
  def apply(f: S[A] => S[A]): AD[S, A] = AD(f(guts))
  def +(that: AD[S, A])(implicit A: Numeric[A])    = AD(mode.plus(this.guts,that.guts))
  def -(that: AD[S, A])(implicit A: Numeric[A])    = AD(mode.minus(this.guts,that.guts))
  def *(that: AD[S, A])(implicit A: Numeric[A])    = AD(mode.times(this.guts,that.guts))
  def /(that: AD[S, A])(implicit A: Fractional[A]) = AD(mode.div(this.guts,that.guts))
}

object AD { 
  def apply[S[_], A](value: S[A])(implicit mode: Mode[S, A]) = new AD[S,A](value)

  class ADNumeric[S[_], A](implicit mode: Mode[S, A], A: Numeric[A]) extends Numeric[AD[S,A]] {
    def compare(a: AD[S, A], b: AD[S, A]) = mode.compare(a.guts, b.guts)
    def plus(a: AD[S, A], b: AD[S, A]): AD[S, A] = AD(mode.plus(a.guts, b.guts))
    def minus(a: AD[S, A], b: AD[S, A]): AD[S, A] = AD(mode.minus(a.guts, b.guts))
    def times(a: AD[S, A], b: AD[S, A]): AD[S, A] = AD(mode.times(a.guts, b.guts))
    def negate(a: AD[S, A]): AD[S, A] = AD(mode.negate(a.guts))
    def fromInt(a: Int) = AD(mode.lift(A.fromInt(a)))
    def toInt(a: AD[S, A]) = mode.toInt(a.guts) // derivative is 0 wherever defined, so this is grudgingly ok
    def toLong(a: AD[S, A]) = mode.toLong(a.guts) // derivative is 0 wherever defined, so this is grudgingly ok
    def toFloat(a: AD[S, A]): Nothing = error("I conscientiously object to giving you this result, despite the API")
    def toDouble(a: AD[S, A]): Nothing = error("I conscientiously object to giving you this result, despite the API")
    override def abs(a: AD[S, A]) = AD(mode.abs(a.guts))
    override def signum(a: AD[S, A]) = mode.signum(a.guts)
  }

  implicit def ADIsNumeric[S[_],A](implicit mode: Mode[S, A], A: Numeric[A]) : Numeric[AD[S,A]] = new ADNumeric[S,A]()

  class ADFractional[S[_], A](implicit mode: Mode[S, A], A: Fractional[A]) extends ADNumeric[S,A] with Fractional[AD[S,A]] {
    def div(a: AD[S, A], b: AD[S, A]): AD[S, A] = AD(mode.div(a.guts,b.guts))
  }

  implicit def ADIsFractional[S[_],A](implicit mode: Mode[S, A], A: Fractional[A]) : Fractional[AD[S,A]] = new ADFractional[S,A]()

  class ADFloating[S[_], A](implicit mode: Mode[S, A], A: Floating[A]) extends ADFractional[S,A] with Floating[AD[S,A]] {
    lazy val pi: AD[S,A] = AD(mode.lift(A.pi))
    def log(a: AD[S,A]): AD[S,A] = AD(mode.log(a.guts))
    def exp(a: AD[S,A]): AD[S,A] = AD(mode.exp(a.guts))
    def sin(a: AD[S,A]): AD[S,A] = AD(mode.sin(a.guts))
    def cos(a: AD[S,A]): AD[S,A] = AD(mode.cos(a.guts))
    override def tan(a: AD[S,A]): AD[S,A] = AD(mode.tan(a.guts))
    def asin(a: AD[S,A]): AD[S,A] = AD(mode.asin(a.guts))
    def acos(a: AD[S,A]): AD[S,A] = AD(mode.acos(a.guts))
    def atan(a: AD[S,A]): AD[S,A] = AD(mode.atan(a.guts))
    def sinh(a: AD[S,A]): AD[S,A] = AD(mode.sinh(a.guts))
    def cosh(a: AD[S,A]): AD[S,A] = AD(mode.cosh(a.guts))
    override def tanh(a: AD[S,A]): AD[S,A] = AD(mode.tanh(a.guts))
  }

  implicit def ADIsFloating[S[_], A](implicit mode: Mode[S, A], A: Floating[A]): Floating[AD[S, A]] = new ADFloating[S, A]()

}
