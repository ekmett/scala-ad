package ad

import scala.collection.mutable.Buffer
import scalaz._
import scalaz.Scalaz._

trait ModeCompanion { 
  def diffa[A:Numeric](f: UU[A]): A => (A, A)
  def diff[A:Numeric](f: UU[A]): A => A = a => diffa(f).apply(a)._2

  // tests
  def test = diffa(new FF[Id,Id,Double] { def apply[S[_]](x: AD[S, Double])(implicit mode: Mode[S, Double]): AD[S, Double] = foo(x) })
  def test2 = diffa(new FF[Id,Id,Double] { def apply[S[_]](x: AD[S, Double])(implicit mode: Mode[S, Double]): AD[S, Double] = cos(foo(x)) })
  def test3 = diffa(new FF[Id,Id,Double] { def apply[S[_]](x: AD[S, Double])(implicit mode: Mode[S, Double]): AD[S, Double] = sin(x) })
}
