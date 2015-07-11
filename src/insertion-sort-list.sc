// https://leetcode.com/problems/insertion-sort-list/
import scala.collection.mutable

// Input and expected output
val input = mutable.LinkedList(6, 5, 3, 1, 8, 7, 2, 4)
val expected = input.sorted

// Arrange
// -------
def sort(list: mutable.LinkedList[Int]): mutable.LinkedList[Int] = {
  for (i <- 1 to list.length - 1) {
    var j = i
    var continue = true
    do {
      if (list(j - 1) > list(j)) swap(list, j - 1, j) else continue = false
      j = j - 1
    } while(continue && j > 0)
  }
  list
}
def swap(list: mutable.LinkedList[Int], posX: Int, posY: Int): Unit = {
  val x = list(posX)
  val y = list(posY)
  list(posX) = y
  list(posY) = x
}

// Act
// ---
val result = sort(input)

// Assert
// ------
assert (expected.equals(result))
assert (expected == result)