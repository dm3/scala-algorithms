package lt.dm3.string

import scala.collection._

object Util {
  def levensteinDistance[T](a: Seq[T], b: Seq[T]): Int = levensteinDistance(a.toArray, b.toArray)

  def levensteinDistance[T](a: Array[T], b: Array[T]): Int = {
    val m = Array(a.length, b.length)
    for (i <- 0 to a.length) m(i).update(0, i)
    for (i <- 0 to b.length) m(0).update(i, i)
    println(m)
    for (i <- 1 to a.length; j <- 1 to b.length) m(i).update(j, min(i, j))
  }

  private def min(i: Int, j: Int): Int = (m(i - 1)(j) + 1) min (m(i)(j-1) + 1) min (m(i-1)(j-1) + eq(i, j))
  private def eq(i: Int, j: Int): Int = if (a(i) == b(j)) 0 else 1
}
