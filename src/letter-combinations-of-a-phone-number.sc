import scala.collection.mutable.ListBuffer

// https://leetcode.com/problems/letter-combinations-of-a-phone-number/
val input = "23"
val expected = Array("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf")

// Arrange
// -------
val maps =
  Map(
    "2" -> "abc",
    "3" -> "def",
    "4" -> "ghi",
    "5" -> "jkl",
    "6" -> "mno",
    "7" -> "pqrs",
    "8" -> "tuv",
    "9" -> "wxyz")

def combine(pressedKeys: String) = {

  def combineRecurse(initial: Array[String], combinator: Array[Char]) = {
    var result = ListBuffer[String]()
    initial.foreach { i =>
      combinator.foreach { c =>
        result += s"$i$c"
      }
    }
    result
  }

  // Assume not all pressed digits can be mapped
  val digits = for {
    digit <- pressedKeys.toCharArray
    if (maps get digit.toString).isDefined
  } yield digit

  var trail = digits.tail
  var result = ListBuffer[String]() ++ maps(digits.head.toString).toCharArray.map(_.toString)
  var continue = true
  do {
    if (trail.nonEmpty) {
      result = combineRecurse(result.toArray, maps(trail.head.toString).toCharArray)
      trail = trail.tail
    } else {
      continue = false
    }
  } while(continue)
  result
}

// Act
// ---
val result = combine(input).toArray

// Assert
// ------
assert (expected.sameElements(result))
assert (expected.deep == result.deep)