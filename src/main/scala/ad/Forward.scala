package ad

import scalaz._
import scalaz.Scalaz._

case class Forward[+A](primal: A, tangent: A) 

object Forward extends ModeCompanion { 
  implicit def ForwardMode[A](implicit A0: Numeric[A]) : Mode[Forward, A] = new Jacobian[Forward, Id, A] {
    val A = A0

    def lift(a: A) = Forward[A](a, A.zero)

/*
    def times(
      a: Forward[A], 
      b: Forward[A]
    ) : Forward[A] = new Forward[A]( 
      A.times(a.primal, b.primal), 
      A.plus(A.times(a.primal, b.tangent), A.times(a.tangent, b.primal))
    )
*/ 

    def D : Mode[Id, A] = Mode.IdMode[A](A)
 
    def primal(a: Forward[A]): A = a.primal

    def unary(f: A => A, dadb : => A, b: Forward[A]) = Forward[A](f(b.primal), A.times(dadb, b.tangent))

    def lift1(f : A => A, df: A => A, b: Forward[A]) = {
      val Forward(pb, db) = b
      val dadb = df(pb)
      Forward[A](f(pb), A.times(dadb, db))
    }

    def lift1_(f: A => A, df: (A,A) => A, b: Forward[A]): Forward[A] = { 
      val Forward(pb, db) = b
      val a = f(pb)
      Forward[A](a, A.times(df(a, pb), db))
    }

    def binary(f: (A,A) => A, dadb: => A, dadc: => A, b: Forward[A], c: Forward[A]): Forward[A] =
      Forward[A](f(b.primal,c.primal), A.plus(A.times(dadb,b.tangent), A.times(dadc,c.tangent)))

    def lift2 (f: (A,A) => A, df: (A,A) => (A,A), b: Forward[A], c: Forward[A]): Forward[A] = { 
      val Forward(pb, db) = b
      val Forward(pc, dc) = c
      val a = f(pb,pc)
      val (dadb, dadc) = df(pb, pc)
      Forward[A](a, A.plus(A.times(dadb,db),A.times(dc,dadc)))
    }

    def lift2_(f: (A,A) => A, df: (A,A,A) => (A,A), b: Forward[A], c: Forward[A]): Forward[A] = { 
      val Forward(pb, db) = b
      val Forward(pc, dc) = c
      val a = f(pb,pc)
      val (dadb, dadc) = df(a, pb, pc)
      Forward[A](a, A.plus(A.times(dadb, db),A.times(dc,dadc)))
    }

    def vdiv(a: Forward[A], b: A)(implicit A: Fractional[A]): Forward[A] =
      Forward[A](A.div(a.primal, b), A.div(a.tangent, b))
    def vtimes(a: Forward[A], b: A): Forward[A] =  
      Forward[A](A.times(a.primal, b), A.times(a.tangent, b))
  }

  def diffa[A](f: UU[A])(implicit A: Numeric[A]) = (x: A) => { 
    val Forward(y, dy) = f(AD(Forward[A](x, A.one))).guts
    (y, dy)
  }
}
