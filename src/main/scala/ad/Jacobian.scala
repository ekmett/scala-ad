package ad

import scala.collection.mutable.Buffer
import scalaz._
import scalaz.Scalaz._

trait Jacobian[S[_], D[_], A] extends Mode[S, A] { 
  // interface
  def D: Mode[D, A]
  def primal(f: S[A]): A
  def unary(f: A => A, dadb : => D[A], b: S[A]): S[A]
  def lift1(f : A => A, df: D[A] => D[A], b: S[A]): S[A]
  def lift1_(f: A => A, df: (D[A],D[A]) => D[A], b: S[A]): S[A]
  def binary(f: (A,A) => A, dadb: => D[A], dadc: => D[A], b: S[A], c: S[A]): S[A]
  def lift2 (f: (A,A) => A, df: (D[A],D[A]) => (D[A],D[A]), b: S[A], c: S[A]): S[A]
  def lift2_(f: (A,A) => A, df: (D[A],D[A],D[A]) => (D[A],D[A]), b: S[A], c: S[A]): S[A]

  // the automatic derivation of automatic differentiation
  def compare(a: S[A], b: S[A]): Int = A.compare(primal(a), primal(b))
  override def abs(a: S[A]): S[A] = if (A.compare(primal(a),A.zero) == -1) negate(a) else a
  override def signum(a: S[A]): Int = A.signum(primal(a))
  def toLong(a: S[A]): Long = A.toLong(primal(a))
  def toInt(a: S[A]): Int = A.toInt(primal(a))
  def toFloat(a: S[A]): Nothing = error("Jacobian.toFloat disallowed")
  def toDouble(a: S[A]): Nothing = error("Jacobian.toDouble disallowed")
  def plus(a: S[A], b: S[A]): S[A] = binary(A.plus(_,_),D.one,D.one,a,b)
  def times(a: S[A], b: S[A]): S[A] = binary(A.times(_,_),D.lift(primal(b)),D.lift(primal(a)),a,b)
  def minus(a: S[A], b: S[A]): S[A] = binary(A.minus(_,_),D.one,D.negate(D.one),a,b)
  def negate(a: S[A]) = unary(A negate _, D.negate(D.one), a)
  def recip(a: S[A])(implicit A: Fractional[A]) = lift1_(x => A.div(A.one, x), (y, x) => D.negate(D.times(y, y)), a)
  def pi(implicit A: Floating[A]): S[A] = lift(A.pi)
  def div(a: S[A], b: S[A])(implicit A: Fractional[A]) = times(a, recip(b))
  def exp(a: S[A])(implicit A: Floating[A]): S[A] = lift1_(A exp _, (y: D[A], x : D[A]) => y, a)
  def log(a: S[A])(implicit A: Floating[A]): S[A] = lift1(A log _, x => D.div(D.one, x), a)
  def sin(a: S[A])(implicit A: Floating[A]): S[A] = lift1(A sin _, D cos _, a)
  def cos(a: S[A])(implicit A: Floating[A]): S[A] = lift1(A cos _, x => D negate (D sin x), a)
  def sinh(a: S[A])(implicit A: Floating[A]): S[A] = lift1(A sinh _, D cosh _, a)
  def cosh(a: S[A])(implicit A: Floating[A]): S[A] = lift1(A cosh _, D sinh _, a)
  def asin(a: S[A])(implicit A: Floating[A]): S[A] = lift1(A asin _, x => D.div(D.one, D.sqrt(D.minus(D.one, D.times(x,x)))), a)
  def acos(a: S[A])(implicit A: Floating[A]): S[A] = lift1(A acos _, x => D.negate(D.div(D.one, D.sqrt(D.minus(D.one, D.times(x,x))))), a)
  def atan(a: S[A])(implicit A: Floating[A]): S[A] = lift1(A atan _, x => D.div(D.one, D.plus(D.one, D.times(x, x))), a)
}
