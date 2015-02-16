import scala.collection.mutable.Buffer

import scalaz._
import scalaz.Scalaz._

package object ad { 
  trait FF[F[_],G[_],A] { 
    def apply[S[_]](f : F[AD[S,A]])(implicit mode: Mode[S,A]): G[AD[S,A]]
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
  def grads[F[_]:Traverse, A:Numeric](f: FU[F, A]): F[A] => Cofree[F,A] = error("TODO")
  def jacobians[F[_]:Traverse, G[_]:Functor, A:Numeric](f: FF[F, G, A]): F[A] => G[Cofree[F,A]] = error("TODO")
  
  implicit def lift[S[_], A](a: A)(implicit mode: Mode[S, A], A: Numeric[A]): AD[S, A] = AD[S,A](mode.lift(a))

  def foo[S[_]](x: AD[S,Double])(implicit mode: Mode[S, Double]): AD[S, Double]  = x * x + fromInt[Double](1)

  // generalized scala.math
  def Pi[A](implicit A: Floating[A]) = A.pi
  def fromInt[A](i: Int)(implicit A: Numeric[A]) = A.fromInt(i)
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
  def signum[A](a: A)(implicit A: Numeric[A]) = A.signum(a)
  def abs[A](a: A)(implicit A: Numeric[A]) = A.abs(a)

  // tests
  def test = diffa(new FF[Id,Id,Double] { def apply[S[_]](x: AD[S, Double])(implicit mode: Mode[S, Double]): AD[S, Double] = foo(x) })
  def test2 = diffa(new FF[Id,Id,Double] { def apply[S[_]](x: AD[S, Double])(implicit mode: Mode[S, Double]): AD[S, Double] = cos(foo(x)) })

}

