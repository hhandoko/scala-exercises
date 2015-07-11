// https://leetcode.com/problems/single-number/

// Input and expected output
val input = Array(4, 5, 2, 1, 5, 4, 1)
val expected = 2

// Arrange
// -------
val sorted = input.sorted

// Act
// ---
val result = sorted.groupBy(identity).collectFirst {
  case (x, ys) if ys.length <= 1 => x
}

// Assert
// ------
assert (expected == result.get)