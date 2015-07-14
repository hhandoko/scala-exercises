// Problem:
//   Divide two integers without using multiplication, division and mod operator.
//   If it is overflow, return MAX_INT.
//
// Source: https://leetcode.com/problems/divide-two-integers/
import scala.util.{Failure, Success, Try}
//
// Input and expected output
// -------------------------
val inputParamDividend = 100
val inputParamDivisor = 5
val expected = 20
//
// Arrange
// -------
/**
 * Divide two integers.
 *
 * @param dividend the number to divide.
 * @param divisor the divisor.
 * @return the division result.
 */
def divide(dividend: Int, divisor: Int): Int = {

  // Continually adding the `divisor` to reach the `dividend` to get the result.
  // Alternatively, we can work backwards from the `dividend` and continually
  // subtracting it with the `divisor` until the result is `0`.

  // Wrap in `Try` to catch overflow error to return Int's MAX_VALUE.
  // Alternatively, Java's `try { ... } catch` can also be used.
  Try({
    var i = divisor
    var j = 0
    do {
      i = i + divisor
      j = j + 1
    } while(i <= dividend)
    j
  }) match {
    case Success(result) => result
    case Failure(fail) => Int.MaxValue
  }

}
//
// Act
// ---
val result = divide(inputParamDividend, inputParamDivisor)
//
// Assert
// ------
assert (expected == result)