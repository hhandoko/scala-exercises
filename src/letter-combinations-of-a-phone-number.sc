// Problem:
//   Given a digit string, return all possible letter combinations that the number could represent.
//   A mapping of digit to letters (just like on the telephone buttons) is given below.
//
//   Phone digits to alphabet keys mapping definition:
//     - 2 -> abc
//     - 3 -> def
//     - 4 -> ghi
//     - 5 -> jkl
//     - 6 -> mno
//     - 7 -> pqrs
//     - 8 -> tuv
//     - 9 -> wxyz
//
// Source: https://leetcode.com/problems/letter-combinations-of-a-phone-number/
import scala.collection.mutable.ListBuffer
//
// Definitions
// -----------
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
//
// Input and expected output
// -------------------------
val input = "23"
val expected = Array("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf")
//
// Arrange
// -------
/**
 * Return all possible letter combinations that the digits could represent.
 *
 * @param digits the pressed phone digits.
 * @return all possible letter combinations.
 */
def letterCombinations(digits: String) = {

  // Use a recursive function with an inner loop to build the array.
  //
  // Visual explanation:
  //   First digit (2):
  //     result -> [a, b, c]
  //   Second digit (3):
  //     result -> [ad, ae, af, bd, be, bf, cd, ce, cf]
  //   Third digit (4):
  //     result -> [
  //                 adg, adh, adi, aeg, aeh, aei, afg, afh, afi,
  //                 bdg, bdh, bdi, beg, beh, bei, bfg, bfh, bfi,
  //                 cdg, cdh, cdi, ceg, ceh, cei, cfg, cfh, cfi
  //               ]

  /**
   * Combine each digits' mapping recursively.
   *
   * @param initial the first digit mapping (i.e. seed).
   * @param combinator the digit mapping to combine (log^n).
   * @return the digits' mapping combination.
   */
  def combineRecurse(initial: Array[String], combinator: Array[Char]) = {
    var result = ListBuffer[String]()
    initial.foreach { i =>
      combinator.foreach { c =>
        result += s"$i$c"
      }
    }
    result
  }

  // We assume not all pressed digits can be mapped, so we skip
  // those that can't be mapped, i.e. filter into a new array.
  val mappedDigits = for {
    digit <- digits.toCharArray
    if (maps get digit.toString).isDefined
  } yield digit

  var trail = mappedDigits.tail
  var result = ListBuffer[String]() ++ maps(mappedDigits.head.toString).toCharArray.map(_.toString)
  do {
    result = combineRecurse(result.toArray, maps(trail.head.toString).toCharArray)
    trail = trail.tail
  } while(trail.nonEmpty)

  result

}
//
// Act
// ---
val result = letterCombinations(input).toArray
//
// Assert
// ------
assert (expected.sameElements(result))
assert (expected.deep == result.deep)