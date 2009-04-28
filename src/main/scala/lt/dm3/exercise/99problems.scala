object 99problems {
  //Find the last element of a list.
  //scala> last(List(1, 1, 2, 3, 5, 8))
  //res0: Int = 8
  def last[T](xs: List[T]): T = xs.last

  // Find the last but one element of a list.
  // scala> penultimate(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 5
  def penultimate[T](xs: List[T]): T = xs.init.last

  // find the Kth element of a list.
  // By convention, the first element in the list is element 0.
  // scala> nth(2, List(1, 1, 2, 3, 5, 8))
  // res0: Int = 2
  def nth[T](index: Int, xs: List[T]): T = xs(index)

  // Find the number of elements of a list.
  // scala> length(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 6
  def length[T](xs: List[T]): Int = xs.length

  // Reverse a list.
  // scala> reverse(List(1, 1, 2, 3, 5, 8))
  // res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse[T](xs: List[T]): List[T] = xs.reverse

  // Find out whether a list is a palindrome.
  // scala> isPalindrome(List(1, 2, 3, 2, 1))
  // res0: Boolean = true
  def isPalindrome[T](xs: List[T]): Boolean = xs.zip(xs.reverse).find(x => x._1 != x._2).isEmpty

  // Flatten a nested list structure.
  // scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  // res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  def flatten(xs: List[Any]): List[Any] = xs.flatMap(x => x match {
    case ls: List[Any] => flatten(ls)
    case n => n :: Nil
  })

  // Eliminate consecutive duplicates of list elements.
  // If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  // scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  import scala.collections._
  def compressAll[T](xs: List[T]): List[T] = {
    //maintain order
    val buf = new mutable.LinkedHashSet[T]
    xs.foreach(buf += _)
    buf.toList
  }
  def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => x :: compress(xs.dropWhile(_ == x))
  }

  // Pack consecutive duplicates of list elements into sublists.
  // If a list contains repeated elements they should be placed in separate sublists.
  // scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs => (List(x) ::: xs.takeWhile(_ == x)) :: pack(xs.dropWhile(_ == x))
  }

  // Run-length encoding of a list.
  // Use the result of problem P09(pack) to implement the so-called run-length
  // encoding data compression method. Consecutive duplicates of elements
  // are encoded as tuples (N, E) where N is the number of duplicates of
  // the element E.
  // scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode[T](xs: List[T]): List[(Int, T)] = pack(xs).map(x => (x.length, x.head))

  // Modified run-length encoding.
  // Modify the result of problem P10 in such a way that if an element has no
  // duplicates it is simply copied into the result list. Only elements with
  // duplicates are transferred as (N, E) terms.
  // scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  def encodeModified[T](xs: List[T]) = encode(xs).map(_ match {
      case (1, x) => x
      case (n, x) => (n, x)
    })

  // Decode a run-length encoded list.
  // Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
  // scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  // res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  def repeat[T](el: T, times: Int): List[T] = times match {
    case 0 => Nil
    case _ => el :: repeat(el, times - 1)
  }
  def decode[T](xs: List[(Int, T)]): List[T] = xs.flatMap(x => repeat(x._2, x._1))

  // Run-length encoding of a list (direct solution).
  // Implement the so-called run-length encoding data compression method
  // directly. I.e. don't use other methods you've written (like P09's pack); do
  // all the work directly.
  // scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encodeDirect[T](xs: List[T]): List[(Int, T)] = xs match {
    case Nil => Nil
    case x :: xs => (xs.takeWhile(_ == x).length + 1, x) :: encodeDirect(xs.dropWhile(_ == x))
  }

  // Duplicate the elements of a list.
  // scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  // res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  def duplicate[T](xs: List[T]) = xs.flatMap(x => x :: x :: Nil)

  //   Duplicate the elements of a list a given number of times.
  // scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  // res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  def duplicateN[T](n: Int, xs: List[T]) = xs.flatMap(repeat(_, n))

  //   Drop every Nth element from a list.
  // scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  def drop[T](n: Int, xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case _ => xs.take(n - 1) ::: drop(n, xs.drop(n))
  }

  // Split a list into two parts.
  // The length of the first part is given. Use a Tuple for your result.
  // scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  def split[T](n: Int, xs: List[T]) = xs.splitAt(n)

  //   Extract a slice from a list.
  // Given two indices, I and K, the slice is the list containing the elements
  // from and including the Ith element up to but not including the Kth element of
  // the original list. Start counting the elements with 0.
  // scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g)
  def slice[T](start: Int, end: Int, xs: List[T]) = xs.slice(start - 1, end - 1)

  //   Rotate a list N places to the left.
  // scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

  // scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  def rotate[T](n: Int, xs: List[T]): List[T] = n match {
    case 0 => xs
    case n if n > 0 => xs.drop(n) ::: xs.take(n)
    case n if n < 0 => rotate(xs.length + n, xs)
  }

  // Remove the Kth element from a list.
  // Return the list and the removed element in a Tuple. Elements are numbered from 0.
  // scala> removeAt(1, List('a, 'b, 'c, 'd))
  // res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  def removeAt[T](n: Int, xs: List[T]) = (xs.take(n) ::: xs.drop(n + 1), xs(n))

  //   Insert an element at a given position into a list.
  // scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  // res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  def insertAt[T](el: T, n: Int, xs: List[T]) = xs.take(n) ::: List(el) ::: xs.drop(n)

  //   Create a list containing all integers within a given range.
  // scala> range(4, 9)
  // res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  def range(from: Int, to: Int) = List.range(from, to + 1)

  import scala.util._
  //   Extract a given number of randomly selected elements from a list.
  // scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  // res0: List[Symbol] = List('e, 'd, 'a)
  def randUniq(rand: Random, max: Int, seen: Set[Int]): Int = rand.nextInt(max) match {
    case n if seen.contains(n) => randUniq(rand, max, seen)
    case n => n
  }
  def randomSelect[T](n: Int, xs: List[T]) = {
    val rand = new scala.util.Random
    var _seen: mutable.HashSet[Int] = mutable.HashSet.empty[Int]
    def loop(num: Int): List[T] = num match {
      case 0 => Nil
      case _ => {
        val next = randUniq(rand, xs.length, _seen)
        _seen += next
        xs(next) :: loop(num - 1)
      }
    }
    loop(n)
  }

  // Lotto: Draw N different random numbers from the set 1..M.
  // scala> lotto(6, 49)
  // res0: List[Int] = List(23, 1, 17, 33, 21, 37)
  def lotto(n: Int, to: Int) = randomSelect(n, range(1, to))

  //   Generate a random permutation of the elements of a list.
  // Hint: Use the solution of problem P23.
  // scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  // res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
  def randomPermute[T](xs: List[T]) = randomSelect(xs.length, xs)

  //   Generate the combinations of K distinct objects chosen from the N elements of a
  // list.
  // In how many ways can a committee of 3 be chosen from a group of 12 people?
  // We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
  //   well-known binomial coefficient). For pure mathematicians, this result may
  // be great. But we want to really generate all the possibilities.
  // scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  // res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  def combinations[T](n: Int, list: List[T]): List[List[T]] = {
    if (n == 0) List(Nil) else list.flatMapSublists((ls) => combinations(n - 1, ls.tail).map(x => ls.head :: x))
  }

  class ExtraRichList[T](list: List[T]) {
    //same as flatMap, but passes successive sublists instead of elements
    //List(1, 2, 3).flatMapSubList will produce List(1, 2, 3,   2, 3,   3)
    def flatMapSublists[U](f: (List[T]) => List[U]): List[U] = {
      def loop(ls: List[T], f: (List[T]) => List[U]): List[U] = ls match {
        case Nil => Nil
        case sublist@(_ :: xs) => f(sublist) ::: loop(xs, f)
      }
      loop(list, f)
    }

    def group(f: T => T => Boolean): List[List[T]] = list match {
      case Nil => Nil
      case x :: xs => {
        val (a, b) = xs.span(f(x))
        (x :: a) :: new ExtraRichList(b).group(f)
      }
    }
  }
  implicit def list2extraRichList[T](list: List[T]) = new ExtraRichList(list)

// (**) Group the elements of a set into disjoint subsets.
//     a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
//     2, 3 and 4 persons? Write a function that generates all the possibilities.
//     scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//     res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...

//     b) Generalize the above predicate in a way that we can specify a list of
//     group sizes and the predicate will return a list of groups.
//     scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//     res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...

//     Note that we do not want permutations of the group members; i.e. ((Aldo,
//     Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a
//     difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David),
//     (Aldo, Beat), ...).
//     You may find more about this combinatorial problem in a good book on
//     discrete mathematics under the term "multinomial coefficients".
  def group[T](groups: List[Int], list: List[T]): List[List[List[T]]] = {
    def loop(_groups: List[Int], _list: List[T]) = _groups match {
      case Nil => Nil
      case x :: xs => groups(xs, combinations(x, list))
    }
  }


  def permutations[T](n: Int, list: List[T]) = list match {
    case Nil => List(List())
    case _ => list.flatMap((elem: T) => permutations[T](list.remove((a: T) => a == elem)) map ((b: List[T]) => elem :: b))
  }
}
