package ad

import scala.collection.mutable.Buffer
import scalaz._
import scalaz.Scalaz._

private [ad] case class Reverse[A](primal: A, slot: Int)

object Reverse { 
  private [ad] trait Entry[+A] 
  private [ad] case object Zero extends Entry[Nothing]
  private [ad] case object Var extends Entry[Nothing]
  private [ad] case class Unary[A](di: A, i: Int) extends Entry[A]
  private [ad] case class Binary[A](di: A, i: Int, dj: A, j: Int) extends Entry[A]

  // TODO: the problem here is the A argument to Tape, we need to coerce all of other arguments to make this happy
  private[ad] class Tape[A](implicit val A: Numeric[A]) extends Mode[Reverse] {
    val buffer = Buffer[Entry[A]](Zero)
    def pushSlot(e: Entry[A]) : Int = synchronized { 
      val len = buffer.length
      buffer += e
      len
    }
    def push(a: A, e: Entry[A]): Reverse[A] = Reverse[A](a, pushSlot(e))

    def fresh(a: A): Reverse[A] = push(a, Var)

    def unary(f: A => A, dadb : => A, b: Reverse[A]) = 
      Reverse[A]( f(b.primal), 
        if (b.slot == 0) 0 
        else pushSlot(Unary[A](dadb, b.slot))
      )

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

    def sensitivities(top : Int)(implicit man: Manifest[A]): Reverse[A] => A = { 
      var result = Array.tabulate[A](top + 1)(n => if (n == top) A.one else A.zero)
      // now iterate backwards across the tape
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
    val minus_one = A.negate(A.one)

    def plus[A](a: Reverse[A], b: Reverse[A])(implicit A: Numeric[A]): Reverse[A]  = todo // binary(A.plus(_,_),A.one,A.one,a,b)
    def times[A](a: Reverse[A], b: Reverse[A])(implicit A: Numeric[A]): Reverse[A] = todo // binary(A.times(_,_),b.primal,a.primal,a,b)
    def minus[A](a: Reverse[A], b: Reverse[A])(implicit A: Numeric[A]): Reverse[A] = todo // binary(A.minus(_,_),A.one,minus_one,a,b)

    def todo = error("todo")
    def compare[A](a: Reverse[A], b: Reverse[A])(implicit A: Numeric[A]) = A.compare(a.primal, b.primal)

    def lift[A](a: A)(implicit A: Numeric[A]) = Reverse[A](a, 0)

    def recip[A](a: Reverse[A])(implicit A: Fractional[A]) = todo
    def negate[A](a: Reverse[A])(implicit A: Numeric[A]) = todo
    def abs[A](a: Reverse[A])(implicit A: Numeric[A]) = {
      if (A.compare(a.primal,A.zero) == -1) negate(a)
      else a
    }

    def signum[A](a: Reverse[A])(implicit A: Numeric[A]) = A.signum(a.primal)
    def toLong[A](a: Reverse[A])(implicit A: Numeric[A]) = A.toLong(a.primal)

    def toInt[A](a: Reverse[A])(implicit A: Numeric[A]) = todo

    def exp[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo
    def log[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo

    def sin[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo
    def cos[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo
    def tan[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo

    def sinh[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo
    def cosh[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo
    def tanh[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo

    def asin[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo
    def acos[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo
    def atan[A](a: Reverse[A])(implicit A: Floating[A]): Reverse[A] = todo
  }

  def diffa[A:Numeric:Manifest](f: UU[A]) : A => (A, A) = (a: A) => {
    val tape = new Tape[A]()
    implicit val mode : Mode[Reverse] = tape
    val x = tape.fresh(a)
    val y = f(AD(x)).guts
    val ybar = tape.sensitivities(y.slot)
    (y.primal, ybar(x))
  }
}
