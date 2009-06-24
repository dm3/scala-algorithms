package lt.dm3.exercise

object arithmetic99 {
  class S99Int(val n: Int) {
    import S99Int._

    // P31 (**) Determine whether a given integer number is prime.
    def isPrime(other: Int) = error("")
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  }
}

