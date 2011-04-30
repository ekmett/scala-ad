package ad

import scala.collection.mutable.Buffer
import scalaz._
import scalaz.Scalaz._

case class Reverse[+A](primal: A, slot: Int)

object Reverse extends ModeCompanion { 
  private [ad] trait Entry[+A] 
  private [ad] case object Zero extends Entry[Nothing]
  private [ad] case object Var extends Entry[Nothing]
  private [ad] case class Unary[+A](di: A, i: Int) extends Entry[A]
  private [ad] case class Binary[+A](di: A, i: Int, dj: A, j: Int) extends Entry[A]

  // TODO: the problem here is the A argument to Tape, we need to coerce all of other arguments to make this happy
  private[ad] class Tape[A](implicit val A: Numeric[A]) extends Jacobian[Reverse, Id, A] { 
    val D : Mode[Id, A] = Mode.IdMode[A]

    def lift(a: A) = Reverse[A](a, 0)
    def primal(a: Reverse[A]): A = a.primal
    val buffer = Buffer[Entry[A]](Zero)
    def pushSlot(e: Entry[A]) : Int = synchronized { 
      val len = buffer.length
      buffer += e
      len
    }
    def push(a: A, e: Entry[A]): Reverse[A] = Reverse[A](a, pushSlot(e))
    def fresh(a: A): Reverse[A] = push(a, Var)

    def vtimes(a: Reverse[A], b: A): Reverse[A] = unary(A.times(_, b), D.lift(b), a)
    def vdiv(a: Reverse[A], b: A)(implicit A: Fractional[A]): Reverse[A] = unary(A.div(_, b), D.lift(A.div(A.one, b)), a)

    def sensitivities(top : Int): Reverse[A] => A = { 
      var result = Buffer.tabulate[A](top + 1)(n => if (n == top) A.one else A.zero)
      top max 1 to 1 by -1 foreach { 
        n => buffer(n) match { 
          case Zero => ()
          case Var => ()
          case Unary(dadb, bix) => result.update(bix, A.plus(result(bix), A.times(dadb,result(n))))
          case Binary(dadb, bix, dadc, cix) => {
            result.update(bix, A.plus(result(bix), A.times(dadb, result(n))))
            result.update(cix, A.plus(result(cix), A.times(dadc, result(n))))
          }
        }
      }
      (x : Reverse[A]) => result(x.slot)
    }

    def unary(f: A => A, dadb : => A, b: Reverse[A]) = 
      Reverse[A]( f(b.primal), 
        if (b.slot == 0) 0 
        else pushSlot(Unary[A](dadb, b.slot))
      )

    def lift1(f : A => A, df: A => A, b: Reverse[A]): Reverse[A] = unary(f, df(b.primal), b)
    def lift1_(f: A => A, df: (A,A) => A, b: Reverse[A]): Reverse[A] = {
      val pb = b.primal
      val a = f(pb)
      unary(_ => a, df(a,pb), b)
    }

    def binary(f: (A, A) => A, dadb: => A, dadc: => A, b: Reverse[A], c: Reverse[A]) = 
      Reverse[A]( f(b.primal,c.primal), 
        if (b.slot == 0) {
          if (c.slot == 0) 0
          else pushSlot(Unary[A](dadc, c.slot))
        } else {
          if (c.slot == 0) pushSlot(Unary[A](dadb, b.slot))
          else pushSlot(Binary[A](dadb, b.slot, dadc, c.slot))
        }
      )
    def lift2(f: (A,A) => A, df: (A,A) => (A,A), b: Reverse[A], c: Reverse[A]): Reverse[A] = { 
      val (dadb, dadc) = df(b.primal, c.primal)
      binary(f, dadb, dadc, b, c)
    }
      
    def lift2_(f: (A,A) => A, df: (A,A,A) => (A,A), b: Reverse[A], c: Reverse[A]): Reverse[A] = {
      val pb = b.primal
      val pc = c.primal
      val a = f(pb, pc)
      val (dadb, dadc) = df(a, pb, pc)
      binary((_,_) => a, dadb, dadc, b, c)
    }
  }

  def diffa[A:Numeric](f: UU[A]) = (a:A) => {
    val tape = new Tape[A]()
    implicit val mode : Mode[Reverse, A] = tape
    val x = tape.fresh(a)
    val y = f(AD(x)).guts
    val ybar = tape.sensitivities(y.slot)
    (y.primal, ybar(x))
  }
}

