// Problem:
//   Given an unsorted array, find the maximum difference between the successive elements in its sorted form.
//   Try to solve it in linear time/space.
//   Return 0 if the array contains less than 2 elements.
//   You may assume all elements in the array are non-negative integers and fit in the 32-bit signed integer range.
//
// Source: https://leetcode.com/problems/maximum-gap/
//
// Input and expected output
// -------------------------
val input = Array(3, 10, 5, 4, 3, 11, 12, 4, 5, 6)
val expected = 4
//
// Arrange
// -------
/**
 * Find the maximum gap between two numbers in an array.
 *
 * @param nums the numbers.
 * @return the maximum gap value.
 */
def maximumGap(nums: Array[Int]): Int = {

  // Use Scala's collection to sort the array, then loop through the array
  // to check for greater maximum gap's value.

  val sorted = nums.sorted
  var max = 0
  sorted.length match {
    // Must contain at least two elements
    case s if s >= 2 => {
      for (i <- 1 to sorted.length - 1) {
        val gap = sorted(i) - sorted(i - 1)
        if (gap > max) { max = gap }
      }
    }
  }

  max

}
//
// Act
// ---
val result = maximumGap(input)
//
// Assert
// ------
assert (expected == result)