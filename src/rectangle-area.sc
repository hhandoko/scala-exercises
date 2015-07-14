// Problem:
//   Find the total area covered by two rectilinear rectangles in a 2D plane.
//   Each rectangle is defined by its bottom left corner and top right corner as shown in the figure.
//
//                  Y
//                  ^
//                  |   (C,D):(3,4)
//            +------------+
//            |     |      |         (G,H):(9,2)
//            |     +------+---------+
//            |     |O(0,0)|         |
//            +-----+------+---------|--> X
// (A,B):(-3,0)     +----------------+
//       (E,F):(0,-1)
//
// Source: https://leetcode.com/problems/rectangle-area/
//
// Input and expected output
// -------------------------
val input = (-3, 0, 3, 4, 0, -1, 9, 2)
val expected = 3 * 2
//
// Arrange
// -------
/**
 * Compute the total area intersection between two rectangles.
 *
 * @param a the rectangle A lower left X coordinate.
 * @param b the rectangle A lower left Y coordinate.
 * @param c the rectangle A upper right X coordinate.
 * @param d the rectangle A upper right Y coordinate.
 * @param e the rectangle B lower left X coordinate.
 * @param f the rectangle B lower left Y coordinate.
 * @param g the rectangle B upper right X coordinate.
 * @param h the rectangle B upper right Y coordinate.
 * @return the total area intersection.
 */
def computeArea(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int): Int = {

  // Identify overlap from each dimension (X and Y), and multiply the result to get
  // the total area.

  /**
   * Get one dimensions' overlap length.
   *
   * @param a1 the rectangle A start coordinate.
   * @param a2 the rectangle A end coordinate.
   * @param b1 the rectangle B start coordinate.
   * @param b2 the rectangle B end coordinate.
   * @return the overlap length.
   */
  def getOverlap(a1: Int, a2: Int, b1: Int, b2: Int): Int = {
    (a1, a2, b1, b2) match {
      // a1---a2 b1---b2
      case _ if a1 <= b1 && a2 < b1 => 0
      // a1---x2y1---b2
      case _ if a1 <= b1 && a2 == b1 => 0
      // a1---b1---b2---a2
      case _ if a1 <= b1 && b2 < a2 => b2 - b1
      // a1---b1---a2---b2
      case _ if a1 <= b1 && b1 < a2 => a2 - b1
      // Invert
      case _ => getOverlap(b1, b2, a1, a2)
    }
  }

  val overlapX = getOverlap(a, c, e, g)
  val overlapY = getOverlap(b, d, f, h)

  overlapX * overlapY

}
//
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
//
// Assert
// ------
assert (expected == result)