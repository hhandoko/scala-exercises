// Problem:
//   Given an array of integers, every element appears twice except for one. Find that single one.
//
// Note:
//   Your algorithm should have a linear runtime complexity. Could you implement it without using extra memory?
//
// Source: https://leetcode.com/problems/single-number/
//
// Input and expected output
// -------------------------
val input = Array(4, 5, 2, 1, 5, 4, 1)
val expected = 2
//
// Arrange
// -------
def singleNumber(nums: Array[Int]): Int = {

  // Scala's collection helps immensely.
  // `collectFirst` is used as we're expecting only one unique element.
  // Otherwise, we can use `collect` and retrieve an array of unique elements.

  nums.groupBy(identity).collectFirst {
    case (x, ys) if ys.length <= 1 => x
  }.get

}
//
// Act
// ---
val result = singleNumber(input)
//
// Assert
// ------
assert (expected == result)