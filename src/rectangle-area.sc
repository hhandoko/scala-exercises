// https://leetcode.com/problems/rectangle-area/

// Input and expected output
val input = (-3, 0, 3, 4, 0, -1, 9, 2)
val expected = 3 * 2

// Arrange
// -------
def computeArea(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int): Int = {

  def getOverlap(x1: Int, x2: Int, y1: Int, y2: Int): Int = {
    (x1, x2, y1, y2) match {
      // x1---x2 y1---y2
      case _ if x1 <= y1 && x2 < y1 => 0
      // x1---x2y1---y2
      case _ if x1 <= y1 && x2 == y1 => 0
      // x1---y1---y2---x2
      case _ if x1 <= y1 && y2 < x2 => y2 - y1
      // x1---y1---x2---y2
      case _ if x1 <= y1 && y1 < x2 => x2 - y1
      // Invert
      case _ => getOverlap(y1, y2, x1, x2)
    }
  }

  val overlapX = getOverlap(a, c, e, g)
  val overlapY = getOverlap(b, d, f, h)
  overlapX * overlapY
}

// Act
// ---
val result =
  computeArea(
    input._1,
    input._2,
    input._3,
    input._4,
    input._5,
    input._6,
    input._7,
    input._8)

// Assert
// ------
assert (expected == result)