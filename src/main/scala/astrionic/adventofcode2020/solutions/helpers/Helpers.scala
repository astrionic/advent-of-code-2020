package astrionic.adventofcode2020.solutions.helpers

private[solutions] object Helpers {
  implicit class Tuple2IntExtensions(private val a: (Int, Int)) extends AnyVal {
    def +(b: (Int, Int)): (Int, Int) = (a._1 + b._1, a._2 + b._2)
    def -(b: (Int, Int)): (Int, Int) = (a._1 - b._1, a._2 - b._2)
    def *(b: Int): (Int, Int) = (a._1 * b, a._2 * b)
  }
}
