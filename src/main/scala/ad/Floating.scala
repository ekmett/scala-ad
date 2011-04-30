package ad

import scala._

trait Floating[A] extends Fractional[A] {
  def one_half: A = div(one, fromInt(2))
  def pi: A
  def exp(a: A): A
  def log(a: A): A
  def sqrt(a: A): A = expBase(a, one_half)
  def logBase(a: A, b: A): A = div(log(b), log(a))
  def expBase(a: A, b: A): A = exp(times(log(a), b))
  def sin(a: A): A
  def cos(a: A): A
  def tan(a: A): A = sin(a) / cos(a)
  def asin(a: A): A
  def acos(a: A): A
  def atan(a: A): A
  def sinh(a: A): A
  def cosh(a: A): A
  def tanh(a: A): A = sinh(a) / cosh(a)
}

object Floating { 
  implicit object DoubleFloating extends Floating[Double] { 
    def div(a: Double, b: Double): Double = a / b
    def toDouble(a: Double): Double = a
    def toFloat(a: Double): Float = a toFloat
    def toLong(a: Double): Long = a toLong
    def toInt(a: Double): Int = a toInt
    def fromInt(a: Int): Double = a
    def negate(a: Double): Double = -a
    def times(a: Double, b: Double) = a * b
    def minus(a: Double, b: Double) = a - b 
    def plus(a: Double, b: Double) = a + b
    def compare(a: Double, b: Double) = 
      if (a < b) -1
      else if (a > b) 1
      else 0
    val pi = math.Pi
    def exp(a: Double) = math.exp(a)
    def log(a: Double) = math.log(a)
    override def sqrt(a: Double) = math.sqrt(a)
    def sin(a: Double) = math.sin(a)
    def cos(a: Double) = math.cos(a)
    override def tan(a: Double) = math.tan(a)
    def asin(a: Double) = math.asin(a)
    def acos(a: Double) = math.acos(a)
    def atan(a: Double) = math.atan(a)
    def sinh(a: Double) = math.sinh(a)
    def cosh(a: Double) = math.cosh(a)
    override def tanh(a: Double) = math.tanh(a)
  }
}
