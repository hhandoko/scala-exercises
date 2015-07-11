// https://leetcode.com/problems/maximum-gap/

// Input and expected output
val input = Array(3, 10, 5, 4, 3, 11, 12, 4, 5, 6)
val expected = 4

// Arrange
// -------
def maximumGap(nums: Array[Int]): Int = {

  val sorted = nums.sorted
  var max = 0
  if (sorted.length > 1) {
    for (i <- 1 to sorted.length - 1) {
      val gap = sorted(i) - sorted(i - 1)
      if (gap > max) { max = gap }
    }
  }
  max

}

// Act
// ---
val result = maximumGap(input)

// Assert
// ------
assert (expected == result)
