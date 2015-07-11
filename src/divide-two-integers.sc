// https://leetcode.com/problems/divide-two-integers/
import scala.util.{Failure, Success, Try}

// Input and expected output
val inputDividend = 100
val inputDivisor = 5
val expected = 20

// Arrange
// -------
def divide(dividend: Int, divisor: Int): Int = {
  Try({
    var i = divisor
    var j = 0
    do {
      i = i + divisor
      j = j + 1
    } while(i <= dividend)
    j
  }) match {
    case Success(res) => res
    case Failure(fail) => Int.MaxValue
  }
}

// Act
// ---
val result = divide(inputDividend, inputDivisor)

// Assert
// ------
assert (expected == result)