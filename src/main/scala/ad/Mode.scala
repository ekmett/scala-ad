package ad

import scala.math._
import scalaz._
import scalaz.Scalaz._

trait Mode[S[_], A] extends Numeric[S[A]] { m => 
  def A: Numeric[A]
  def lift(a: A): S[A]

  def fromInt(i: Int): S[A] = lift(A fromInt i)

  def vtimes(a: S[A], b: A): S[A]
  // def timesv(a: A, b: S[A]): S[A]

  def div(a: S[A], b: S[A])(implicit A: Fractional[A]): S[A]
  def vdiv(a: S[A], b: A)(implicit A: Fractional[A]): S[A]

  def pi(implicit A: Floating[A]): S[A]
  def exp(a: S[A])(implicit A: Floating[A]): S[A]
  def log(a: S[A])(implicit A: Floating[A]): S[A]
  def sin(a: S[A])(implicit A: Floating[A]): S[A]
  def cos(a: S[A])(implicit A: Floating[A]): S[A]
  def tan(a: S[A])(implicit A: Floating[A]): S[A] = div(sin(a), cos(a))
  def sinh(a: S[A])(implicit A: Floating[A]): S[A]
  def cosh(a: S[A])(implicit A: Floating[A]): S[A]
  def tanh(a: S[A])(implicit A: Floating[A]): S[A] = div(sinh(a), cosh(a))
  def asin(a: S[A])(implicit A: Floating[A]): S[A]
  def acos(a: S[A])(implicit A: Floating[A]): S[A]
  def atan(a: S[A])(implicit A: Floating[A]): S[A]

  def toFractional(implicit A: Fractional[A]): Fractional[S[A]] = new Mode.FractionalModeProxy[S,A](this,A)
  def toFloating(implicit A: Floating[A]): Floating[S[A]] = new Mode.FloatingModeProxy[S,A](this,A)
}

object Mode {
  class FractionalModeProxy[S[_],A](S: Mode[S,A], A: Fractional[A]) extends Fractional[S[A]] { 
    override def abs(a: S[A]): S[A] = S.abs(a)
    override def signum(a: S[A]): Int = S.signum(a)
    def compare(a: S[A], b: S[A]): Int = S.compare(a, b)
    def plus(a: S[A], b: S[A]): S[A] = S.plus(a, b)
    def times(a: S[A], b: S[A]): S[A] = S.times(a, b)
    def minus(a: S[A], b: S[A]): S[A] = S.minus(a, b)
    def negate(a: S[A]): S[A] = S.negate(a)
    def fromInt(i: Int): S[A] = S.fromInt(i)
    def toInt(a: S[A]): Int = S.toInt(a)
    def toLong(a: S[A]): Long = S.toLong(a)
    def toFloat(a: S[A]): Float = S.toFloat(a)
    def toDouble(a: S[A]): Double = S.toDouble(a)
    def div(a: S[A], b: S[A]): S[A] = S.div(a, b)(A)
  }
  implicit def FractionalMode[S[_],A](S: Mode[S,A])(implicit A: Fractional[A]): Fractional[S[A]] = S.toFractional

  class FloatingModeProxy[S[_],A](S: Mode[S,A], A: Floating[A]) extends FractionalModeProxy[S,A](S,A) with Floating[S[A]] {
    def pi: S[A] = S.pi(A)
    def exp(a: S[A]): S[A] = S.exp(a)(A)
    def log(a: S[A]): S[A] = S.log(a)(A)
    def sin(a: S[A]): S[A] = S.sin(a)(A)
    def cos(a: S[A]): S[A] = S.cos(a)(A)
    override def tan(a: S[A]): S[A] = S.tan(a)(A)
    def sinh(a: S[A]): S[A] = S.sinh(a)(A)
    def cosh(a: S[A]): S[A] = S.cosh(a)(A)
    override def tanh(a: S[A]): S[A] = S.tanh(a)(A)
    def asin(a: S[A]): S[A] = S.asin(a)(A)
    def acos(a: S[A]): S[A] = S.acos(a)(A)
    def atan(a: S[A]): S[A] = S.atan(a)(A)
  }
  implicit def FloatingMode[S[_],A](S: Mode[S,A])(implicit A: Floating[A]): Floating[S[A]] = S.toFloating

  def IdMode[A](implicit num: Numeric[A]) : Mode[Id, A] = new Mode[Id, A] { 
    val A = num
    def lift(a: A): A = a
    def compare(a: A, b: A): Int = A.compare(a, b)
    def plus(a: A, b: A): A  = A.plus(a, b)
    def times(a: A, b: A): A = A.times(a, b)
    def minus(a: A, b: A): A = A.minus(a, b)
    override def abs(a: A): A = A abs a
    override def signum(a: A): Int = A signum a
    def toLong(a: A): Long = A toLong a
    def toInt(a: A): Int = A toInt a 
    def toFloat(a: A): Float = A toFloat a
    def toDouble(a: A): Double = A toDouble a
    def negate(a: A) = A negate a
    def pi(implicit A: Floating[A]): A = A.pi
    def div(a: A, b: A)(implicit A: Fractional[A]): A = A.div(a,b)
    def exp(a: A)(implicit A: Floating[A]): A = A exp a
    def log(a: A)(implicit A: Floating[A]): A = A log a
    def sin(a: A)(implicit A: Floating[A]): A = A sin a
    def cos(a: A)(implicit A: Floating[A]): A = A cos a
    override def tan(a: A)(implicit A: Floating[A]): A = A tan a
    def sinh(a: A)(implicit A: Floating[A]): A = A sinh a
    def cosh(a: A)(implicit A: Floating[A]): A = A cosh a
    override def tanh(a: A)(implicit A: Floating[A]): A = A tanh a
    def asin(a: A)(implicit A: Floating[A]): A = A asin a
    def acos(a: A)(implicit A: Floating[A]): A = A acos a
    def atan(a: A)(implicit A: Floating[A]): A = A atan a
    def vtimes(a: A, b: A): A = A.times(a,b)
    def vdiv(a: A, b: A)(implicit A: Fractional[A]): A = A.div(a,b)
    override def toFractional(implicit A: Fractional[A]): Fractional[A] = A
    override def toFloating(implicit A: Floating[A]): Floating[A] = A
  }
}
