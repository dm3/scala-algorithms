package lt.dm3.cormen

import scala.collection._
object Sorting {

  def main(as: Array[String]) {
    val xs = List(6, 4, 2, 1, 3, 5, 2, 2, 35, 34)
    println(Imperative insertion xs)
    println(Imperative merge xs)
    println(Imperative.quicksort normal xs)
    println(Imperative.quicksort randomized xs)
  }

  object Search {
    /*
     * @return index of the place key should be inserted into
     */
    def binary[T <% Ordered[T]](key: T, xs: Seq[T]): Int = {
      val p = (x: T) => key > x
      xs match {
        case Nil => 0
        case x :: Nil => if (p(x)) 1 else 0
        case _ => {
          val middle = Math.ceil(xs.length / 2d).toInt
          val head = xs.slice(0, middle)
          val tail = xs.slice(middle, xs.length)
          if (p(head.last) && !p(tail.first))
            // lastIndexOf is needed because there might be duplicate elements in the list
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

    def swap[T](as: mutable.ListBuffer[T], i: Int, j: Int) {
      var tmp = as(i); as(i) = as(j); as(j) = tmp
    }
    
    /**
    * Straight adaptation from Cormen.
    * O(n^2) worst case, O(n) best case
    */
    def insertion[T <% Ordered[T]](as: List[T]): List[T] = {
      var ret: mutable.ListBuffer[T] = new mutable.ListBuffer
      as copyToBuffer ret
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
    def insertionB[T <% Ordered[T]](as: List[T]): List[T] = {
      var ret: mutable.ListBuffer[T] = new mutable.ListBuffer
      as copyToBuffer ret
      var i = 0
      for (j <- 1 to ret.length - 1; val key = ret(j)) {
        //ret.insertAt(Search.binary(key, ret), as(j))
      }
      ret.toList
    }

    object quicksort {
      import scala.util.Random
	    def normal[T <% Ordered[T]](as: List[T]) = {
	      quicksort(as, (start: Int, end: Int) => start)
	    }
	    
	    /**
         * Randomizing pivot is the same as randomizing the array itself (see introduction to algorithms MIT lecture 4 (year 2005)).
         * 
	     * @param as List to be sorted
	     * @return
	     */
	    def randomized[T <% Ordered[T]](as: List[T]) = {
	      quicksort(as, (start: Int, end: Int) => (new Random).nextInt(end - start) + start)
	    }
	    
	    /**
	     * Straight adaptation from Cormen.
	     * O(n^2) worst case, O(n*lgn) best case
	     * 
	     * @param as List to be sorted
	     * @param as list to sort
	     * @param pivot function to get a pivot with
	     * @return
	     */
	    private def quicksort[T <% Ordered[T]](as: List[T], pivot: (Int, Int) => Int) = {
	      def partition(as: mutable.ListBuffer[T], p: Int, q: Int) = {
            swap(as, pivot(p, q), p) // so that pivot would always be at the start of the list
	        val x = as(p)
	        var i = p
	        for (j <- p + 1 to q if as(j) <= x) { 
	          i += 1
	          swap(as, i, j)
	        }
	        swap(as, i, p) // put pivot in the middle
	        i
	      }
	      
	      def quicksortInternal(as: mutable.ListBuffer[T], p: Int, q: Int) {
	        if (p < q) {
		      var r = partition(as, p, q)
		      quicksortInternal(as, p, r - 1)
		      quicksortInternal(as, r + 1, q)
	        }
	      }
	         
	      var ret: mutable.ListBuffer[T] = new mutable.ListBuffer
	      as copyToBuffer ret
	      quicksortInternal(ret, 0, ret.size - 1)
	      ret.toList;
	    }
    }
    
    
    /**
     * Straight adaptation from Cormen.
     * O(n*lgn) worst case, O(n*lgn) best case
     * @param as List to be sorted
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
      as copyToBuffer ret
      mergeInternal(ret, 0, ret.size).toList
    }

  }

  object Functional {
    /**
    * insertion sort is destructing by nature, requires mutable structures
    */
    def insertion[T <% Ordered[T]](as: List[T]) = as.sort(_ < _)
  }
}
