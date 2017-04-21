// Problem:
//   Divide two integers without using multiplication, division and mod operator.
//   If it is overflow, return MAX_INT.
//
// Source: https://leetcode.com/problems/divide-two-integers/
import scala.annotation.tailrec
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

  // TODO: How to deal with negative inputs (dividend / divisor)

  /**
   * Tail-recursive division.
   *
   * Continually adding the `divisor` to reach the `dividend` to get the result.
   * Alternatively, we can work backwards from the `dividend` and continually
   * subtracting it with the `divisor` until the result is `0`.
   *
   * @param acc the accumulator.
   * @param i the current iterator value.
   * @return the division result.
   */
  @tailrec
  def div(acc: Int, i: Int): Int = {
    if (i > dividend) acc
    else div(acc + 1, i + divisor)
  }

  if (dividend == 0 || divisor == 0) Int.MaxValue
  else div(0, divisor)
}
//
// Act
// ---
val result = divide(inputParamDividend, inputParamDivisor)
//
// Assert
// ------
assert (expected == result)