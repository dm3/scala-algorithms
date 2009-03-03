import scala.collection._

/**
 * Lempel-Ziv-Welsch data compression algorithm.  Can be used to determine
 * frequency of the substring in a string (checkout the encoding table after
 * encoding is finished).
*/
object LZW {
  def main(args: Array[String]) = {
    println(encode ("TOBEORNOTTOBE#"))
  }

  def encode(str: String) = new LZWEncoder(str).encode
}

/**
* Step 1. 	Initialize dictionary to contain one entry for each byte.
*               Initialize the encoded string with the first byte of the input stream.
* Step 2. 	Read the next byte from the input stream.
* Step 3. 	If the byte is an EOF goto step 6.
* Step 4. 	If concatenating the byte to the encoded string produces a string that is in the dictionary:
*
*     * concatenate the the byte to the encoded string
*     * go to step 2
*
* Step 5. 	If concatenating the byte to the encoded string produces a string that is not in the dictionary:
*
*     * add the new sting to the dictionary
*     * write the code for the encoded string to the output stream
*     * set the encoded string equal to the new byte
*     * go to step 2
*
* Step 6. 	Write out code for encoded string and exit.
*/
class LZWEncoder(val initial: String) {
  //linked hashmap maintains order of elements
  val table: mutable.LinkedHashMap[String, Int] = initTable(initial)
  val output: StringBuilder = new StringBuilder

  /**
  * Constructs a unique character table out of the supplied string.
  */
  def initTable(str: String) = {
    val map = new mutable.LinkedHashMap[String, Int]
    // toList produces list of Char => cast to String
    str.toList.removeDuplicates.zipWithIndex.foreach(x => map += (x._1.toString -> x._2))
    map
  }

  /**
  * imperative algorithm
  */
  def encode = {
    var word = ""
    initial.foreach(char => {
      val chars = word + char.toString
      if (table contains chars) word = chars
      else {
        val index = table.size + 1
        table += (word -> index)
        output += index.toChar
      }
    })
    output
  }
}

