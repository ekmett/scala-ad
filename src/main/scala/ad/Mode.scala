package ad

import scala.math._

trait Mode[S[_], A] {
  def lift(a: A)(implicit A: Numeric[A]): S[A]
  def compare(a: S[A], b: S[A])(implicit A: Numeric[A]): Int // GAH!
  def abs(a: S[A])(implicit A: Numeric[A]): S[A]
  def signum(a: S[A])(implicit A: Numeric[A]): Int // GAH!
  def toInt(a: S[A])(implicit A: Numeric[A]): Int // GAH!
  def toLong(a: S[A])(implicit A: Numeric[A]): Long // GAH!
  def plus(a: S[A], b: S[A])(implicit A: Numeric[A]): S[A]
  def minus(a: S[A], b: S[A])(implicit A: Numeric[A]): S[A]
  def times(a: S[A], b: S[A])(implicit A: Numeric[A]): S[A]
  def negate(a: S[A])(implicit A: Numeric[A]): S[A]

  def div(a: S[A], b: S[A])(implicit A: Fractional[A]): S[A] = times(a, recip(b))
  def recip(a: S[A])(implicit A: Fractional[A]): S[A]

  def log(a: S[A])(implicit A: Floating[A]): S[A]
  def exp(a: S[A])(implicit A: Floating[A]): S[A]

  def sin(a: S[A])(implicit A: Floating[A]): S[A]
  def cos(a: S[A])(implicit A: Floating[A]): S[A]
  def tan(a: S[A])(implicit A: Floating[A]): S[A] = div(sin(a), cos(a))

  def sinh(a: S[A])(implicit A: Floating[A]): S[A]
  def cosh(a: S[A])(implicit A: Floating[A]): S[A]
  def tanh(a: S[A])(implicit A: Floating[A]): S[A] = div(sinh(a), cosh(a))

  def asin(a: S[A])(implicit A: Floating[A]): S[A]
  def acos(a: S[A])(implicit A: Floating[A]): S[A]
  def atan(a: S[A])(implicit A: Floating[A]): S[A]
}

