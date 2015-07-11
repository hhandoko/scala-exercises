import scala.util.{Failure, Success, Try}

// https://leetcode.com/problems/divide-two-integers/
// Input and expected output
val input = 100
val divisor = 5
val expected = 20

// Arrange
// -------
def divide(x: Int, y: Int): Int = {
  Try({
    var i = y
    var j = 0
    var continue = true
    do {
      if (i == x) continue = false
      i = i + y
      j = j + 1
    } while(continue)
    j
  }) match {
    case Success(res) => res
    case Failure(fail) => Int.MaxValue
  }
}

// Act
// ---
val result = divide(input, divisor)

// Assert
// ------
assert (expected == result)