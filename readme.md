**Usage**

With sbt:

```scala
$ sbt console
...
scala> import scalaz._
scala> import scalaz.Scalaz._
scala> import ad._

scala> def test = diffa(new FF[Id,Id,Double] {
     | def apply[S[_]](x: AD[S, Double])(implicit mode: Mode[S, Double]): AD[S, Double] = sin(x)
     | })

scala> test(0)
scala> res11: (Double, Double) = (0.0,1.0)
```
