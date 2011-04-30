import scala.collection.mutable.Buffer

import scalaz._
import scalaz.Scalaz._

package object ad { 
  trait Mode[S[_]] {
    def lift[A:Numeric](a: A): S[A]
    def compare[A:Numeric](a: S[A], b: S[A]): Int // GAH! Crappy scala API! 
    def abs[A:Numeric](a: S[A]): S[A]
    def signum[A:Numeric](a: S[A]): Int // GAH! Crappy scala API!
    def toInt[A:Numeric](a: S[A]): Int // GAH! Crappy scala API!
    def toLong[A:Numeric](a: S[A]): Long // GAH! Crappy scala API!
    def plus[A:Numeric](a: S[A], b: S[A]): S[A]
    def minus[A:Numeric](a: S[A], b: S[A]): S[A]
    def times[A:Numeric](a: S[A], b: S[A]): S[A]
    def negate[A:Numeric](a: S[A]): S[A]
    def div[A:Fractional](a: S[A], b: S[A]): S[A] = times(a, recip(b))
    def recip[A:Fractional](a: S[A]): S[A]
    def log[A:Floating](a: S[A]): S[A]
    def exp[A:Floating](a: S[A]): S[A] 
    // def sqrt[A:Floating](a: S[A]): S[A]
    // def logBase[A:Floating](a: S[A], b: S[A]): S[A]
    // def expBase[A:Floating](a: S[A], b: S[A]): S[A] 
    def sin[A:Floating](a: S[A]): S[A]
    def cos[A:Floating](a: S[A]): S[A]
    def tan[A:Floating](a: S[A]): S[A]
    def asin[A:Floating](a: S[A]): S[A]
    def acos[A:Floating](a: S[A]): S[A]
    def atan[A:Floating](a: S[A]): S[A]
    def sinh[A:Floating](a: S[A]): S[A]
    def cosh[A:Floating](a: S[A]): S[A]
    def tanh[A:Floating](a: S[A]): S[A]
  }
  
  trait FF[F[_],G[_],A] { 
    def apply[S[_]:Mode](f : F[AD[S,A]]): G[AD[S,A]]
  }

  type UU[A] = FF[Id, Id, A]
  type FU[F[_],A] = FF[F, Id, A]
  type UF[F[_],A] = FF[Id, F, A]

  def diffa[A](f: UU[A])(implicit A: Numeric[A]) = (x: A) => {
    val Forward(y, dy) = f(AD(Forward[A](x, A.one))).guts
    (y, dy)
  }

  def grada[F[_]:Traverse, A:Numeric](f: FU[F, A]): F[A] => (A, F[A]) = error("TODO")
  def grad[F[_]:Traverse, A:Numeric](f: FU[F, A]): F[A] => F[A] = error("TODO")
  def grads[F[_]:Traverse, A:Numeric](f: FU[F, A]): F[A] => Cofree_[F,A] = error("TODO")
  def jacobians[F[_]:Traverse, G[_]:Functor, A:Numeric](f: FF[F, G, A]): F[A] => G[Cofree_[F,A]] = error("TODO")
  
  implicit def lift[S[_]:Mode, A:Numeric](a: A): AD[S, A] = AD[S,A](implicitly[Mode[S]].lift(a))

  def foo[S[_]:Mode](x: AD[S,Double]): AD[S, Double]  = x * x + implicitly[Numeric[Double]].fromInt(1)

  def test = diffa(new FF[Id,Id,Double] { def apply[S[_]:Mode](x: AD[S, Double]): AD[S, Double] = foo(x) })
  def test2 = diffa(new FF[Id,Id,Double] { def apply[S[_]:Mode](x: AD[S, Double]): AD[S, Double] = cos(foo(x)) })

  def Pi[A](implicit A: Floating[A]) = A.pi
  def exp[A](a: A)(implicit A: Floating[A]): A = A.exp(a)
  def log[A](a: A)(implicit A: Floating[A]): A = A.log(a)
  def sqrt[A](a: A)(implicit A: Floating[A]): A = A.sqrt(a)
  def logBase[A](a: A, b: A)(implicit A: Floating[A]): A = A.logBase(a, b)
  def expBase[A](a: A, b: A)(implicit A: Floating[A]): A = A.expBase(a, b)
  def sin[A](a: A)(implicit A: Floating[A]): A = A.sin(a)
  def cos[A](a: A)(implicit A: Floating[A]): A  = A.cos(a)
  def tan[A](a: A)(implicit A: Floating[A]): A = A.tan(a)
  def asin[A](a: A)(implicit A: Floating[A]): A = A.asin(a)
  def acos[A](a: A)(implicit A: Floating[A]): A = A.acos(a)
  def atan[A](a: A)(implicit A: Floating[A]): A = A.atan(a)
  def sinh[A](a: A)(implicit A: Floating[A]): A = A.sinh(a)
  def cosh[A](a: A)(implicit A: Floating[A]): A = A.cosh(a)
  def tanh[A](a: A)(implicit A: Floating[A]): A = A.tanh(a)
  // def asinh[A](a: A)(implicit A: Floating[A]): A = A.asinh(a)
  // def acosh[A](a: A)(implicit A: Floating[A]): A = A.acosh(a)
  // def atanh[A](a: A)(implicit A: Floating[A]): A = A.atanh(a)
}

/*
case class Reverse[X, @specialized A](primal: A, slot: Int)

object Reverse { 
  class Tape[@specialized A]()(implicit val A: Numeric[A]) {
    val buffer = Buffer[Entry[A]](Zero)
    implicit val ops : Numeric.Ops[A] = A.ops
    def sensitivities(top : Int): Array[A] = { 
      var result = Array.tabulate[A](top + 1)(n => if (n == top) A.one else A.zero)
      // now iterate backwards across the tape
      top max 1 to 1 by -1 foreach { 
        n => buffer(n) match { 
          case Zero => ()
          case Var => ()
          case Unary(dadb, bix) => result.update(bix, result(bix) + dadb * result(n))
          case Binary(dadb, bix, dadc, cix) => {
            result.update(bix, result(bix) + dadb * result(n))
            result.update(cix, result(cix) + dadc * result(n))
          }
        }
      }
      (x : Reverse[A]) => result(x.slot)
    }
    def pushSlot(e: Entry[A]) : Int = synchronized { 
      val len = buffer.length
      buffer += e
      len
    }
    def push(a: A, e: Entry[A]): Int = Reverse[A](a, pushEntry(e))

    def unary(f: A => A, dadb : => A, b: Reverse[A]) = 
      Reverse[A]( f(b.primal), 
        if (b.slot == 0) 0 
        else push(Unary[A](dadb, b.slot))
      )

    def binary(f: (A,A) => A, dadb: => A, dadc: => A, b: Reverse[A], c: Reverse[A]) = 
      Reverse[A]( f(b.primal,c.primal), 
        if (b.slot == 0) {
          if (c.slot == 0) 0
          else pushSlot(Unary[A](dadc, c.slot))
        } else {
          if (c.slot == 0) pushSlot(Unary[A](dadb, b.slot))
          else pushSlot(Binary[A](dadb, b.slot, dadc, c.slot))
        }
      )
    def fresh(a: A): Reverse[A] = push(a, Var)
  }

  object Tape { 
    trait Entry[@specialized +A] 
    case object Zero extends Entry[Nothing]
    case object Var extends Entry[Nothing]
    case class Unary[@specialized A](di: A, i: Int) extends Entry[A]
    case class Binary[@specialized A](di: A, i: Int, dj: A, j: Int) extends Entry[A]
  }

  implicit def lift[A:Numeric](a: A) = Reverse[A](a, implicitly[Numeric[A]].zero[A])

  def diff[A:Numeric](f: Reverse[A] => Reverse[A]) : A => (A, A) = (a: A) => {
    val tape = new Tape()
    val x = tape.fresh(a)
    val y = f(x)
    val ybar = tape.sensitivities(y.slot)
    (y.primal, ybar(x))
  }

  class ReverseNumeric[@specialized A](tape: Tape[A]) extends Numeric[Reverse[A]] with Mode[A] {
    def num: Numeric[A]
    def minus_one = num.negate(num.one)
    def plus(a: Reverse[A], b: Reverse[A]): Reverse[A] = tape.binary(num.plus(_,_),num.one,num.one,a,b)
    def times(a: Reverse[A], b: Reverse[A]): Reverse[A] = tape.binary(num.times(_,_),b.primal,a.primal,a,b)
    def minus(a: Reverse[A], b: Reverse[A]): Reverse[A] = tape.binary(num.minus(_,_),num.one,minus_one,a,b)
    // def abs(a: Reverse[A]): Reverse[A] = unary(num.abs(_),
  }

}
*/
