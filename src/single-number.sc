// https://leetcode.com/problems/single-number/

// Input and expected output
val input = Array(4, 5, 2, 1, 5, 4, 1)
val expected = 2

// Arrange
// -------
def singleNumber(nums: Array[Int]): Int = {

  nums.sorted.groupBy(identity).collectFirst {
    case (x, ys) if ys.length <= 1 => x
  }.get

}

// Act
// ---
val result = singleNumber(input)

// Assert
// ------
assert (expected == result)