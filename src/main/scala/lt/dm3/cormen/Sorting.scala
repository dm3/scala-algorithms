package lt.dm3.cormen

import scala.collection._
object Sorting {

  def main(as: Array[String]) {
    println(Imperative insertion List(3, 2, 1))
    println(Imperative merge List(6, 4, 2, 1))
  }

  object Search {
    def linear[T <% Ordered[T]](p: T => Bool, as: List[T]) = {
      // while (i >= 0 && ret(i) > key) {
      //   ret(i + 1) = ret(i)
      //   i = i - 1
      // }
    }

    /*
     * @return index of the place key should be inserted into
     */
    def binary[T <% Ordered[T]](key: T, xs: Seq[T]): Int = {
      val p = (x: T) => if (key > x) true else false
      xs match {
        case Nil => 0
        case x :: Nil => if (p(x)) 1 else 0
        case _ => {
          val middle = Math.ceil(xs.length / 2d).toInt
          val head = xs.slice(0, middle)
          val tail = xs.slice(middle, xs.length)
          if (p(head.last) && !p(tail.first))
            xs.lastIndexOf(head.last) + 1
          else if (p(tail.first))
            binary(key, tail) // key should be placed somewhere in the tail
          else
            binary(key, head) // key should be placed somewhere in the head
        }
      }
    }
  }

  object Imperative {

    /**
    * Straight adaptation from Cormen.
    * O(n^2) worst case, O(n) best case
    */
    def insertion[T <% Ordered[T]](as: List[T]): List[T] = {
      var ret: mutable.ListBuffer[T] = new mutable.ListBuffer
      ret.insertAll(0, as)
      var i = 0
      for (j <- 1 to ret.length - 1; val key = ret(j)) {
        i = j - 1
        while (i >= 0 && ret(i) > key) {
          ret(i + 1) = ret(i)
          i = i - 1
        }
        ret(i + 1) = key
      }
      ret.toList
    }

    // 2.3-6 Insertion sort with binary search
    def insertion[T <% Ordered[T]](as: List[T]): List[T] = {
      var ret: mutable.ListBuffer[T] = new mutable.ListBuffer
      ret.insertAll(0, as)
      var i = 0
      for (j <- 1 to ret.length - 1; val key = ret(j)) {
        ret.insertAt(Search.binary(key, ret))
      }
      ret.toList
    }
//    def recursiveInsertion[T <% Ordered[T]](as: List[T]) = {
//      def loop(as: mutable.ListBuffer[T], n: Int) = n match {
//        case 1 => as
//        case _ => {
//          var key = as(n)
//          loop(as, n - 1).take(n).zipWithIndex.find(_._1 < key).foreach(as.insert(i))
//          for (i <- 0 to n) {
//            if (as(i) < key) {
//              as.insert(i, key)
//            }
//          }
//        }
//      }
//        }
//
//      val ret = new mutable.ListBuffer[T]
//      ret.insertAll(0, as)
//      loop(ret, ret.size).toList
//    }

    /**
     * Straight adaptation from Cormen.
     * O(n) worst case, O(n) best case
     * @param as List to be sorted
     * @param p - start index
     * @param r - end index
     */
    def merge[T <% Ordered[T]](as: List[T]): List[T] = {
      def mergeInternal(as: mutable.ListBuffer[T], p: Int, r: Int): mutable.ListBuffer[T] = {
	    // xs[p..q] + xs[q + 1..r]
	    def mergeHalves(xs: mutable.ListBuffer[T], p: Int, q: Int, r: Int) {
	      val n = q - p
	      val m = r - q
	      val left = xs.slice(p, p + n + 1)
	      val right = xs.slice(q + 1, r + 1)
	      var i = 0
	      var j = 0
	      for (k <- p to r) {
	        if (i != left.size && (j == right.size || left(i) <= right(j))) {
	          xs(k) = left(i)
	          i = i + 1
	        } else if (j != right.size) {
	          xs(k) = right(j)
	          j = j + 1
	        }
	      }
	    }

	    if (p < r) {
	      //what happens if p + r overflows?
	      val q = Math.floor((p + r) / 2).toInt
	      mergeInternal(as, p, q)
	      mergeInternal(as, q + 1, r)
	      mergeHalves(as, p, q, r)
	    }
        as
      }

      val ret = new mutable.ListBuffer[T]
      ret.insertAll(0, as)
      mergeInternal(ret, 0, ret.size).toList
    }

  }

  object Functional {
    /**
    * insertion sort is destructing by nature, requires mutable structures
    */
    def insertion[T <% Ordered[T]](as: List[T]): List[T] = as.sort(_ < _)
  }
}
