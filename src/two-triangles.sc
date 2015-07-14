// Problem:
//   Write a function that receives three integer inputs for the lengths of the sides of a triangle and returns one of four values to determine the triangle type.
//     1. Scalene     = No equal sides
//     2. Isosceles   = Two equal sided
//     3. Equilateral = Three equal sides
//     4. Error       = Not a valid triangle
//
// Definition
// ----------
//
// Triangle types enumeration
sealed trait TriangleTypes { def name: String}
object TriangleTypes {
  case object SCALENE extends TriangleTypes { val name = "Scalene" }
  case object ISOSCELES extends TriangleTypes { val name = "Isosceles" }
  case object EQUILATERAL extends TriangleTypes { val name = "Equilateral" }
  case object ERROR extends TriangleTypes { val name = "Error" }
}
//
// Input and expected output
// -------------------------
val inputScalene = (3, 5, 7)
val inputIsosceles = (3, 3, 5)
val inputEquilateral = (3, 3, 3)
val inputError = (2, 3, 7)
//
// Arrange
// -------
/**
 * Get a triangle type from the length of its sides.
 *
 * @param a the first side length.
 * @param b the second side length.
 * @param c the third side length.
 * @return the triangle type.
 */
def getTriangleTypes(a: Int, b: Int, c: Int): TriangleTypes = {

  // Use Scala's collection `distinct` method to get the number of equal sides.

  /**
   * Check for a valid triangle (sum of two sides must be greater than the third side).
   *
   * @param x the first side length.
   * @param y the second side length.
   * @param z the third side length.
   * @return true if valid triangle.
   */
  def assertValidTriangle(x: Int, y: Int, z: Int): Boolean = {
    // Check if length is valid
    x > 0 && y > 0 && z > 0 &&
    // Check if the sum of two sides is greater than the third
    x + y > z && y + z > x && z + x > y
  }

  if (assertValidTriangle(a, b, c)) {
    Array(a, b, c).distinct.length match {
      case x if x == 1 => TriangleTypes.EQUILATERAL
      case x if x == 2 => TriangleTypes.ISOSCELES
      case _ => TriangleTypes.SCALENE
    }
  } else {
    TriangleTypes.ERROR
  }

}
//
// Act
// ---
val resultScalene = getTriangleTypes(inputScalene._1, inputScalene._2, inputScalene._3)
val resultIsosceles = getTriangleTypes(inputIsosceles._1,inputIsosceles._2, inputIsosceles._3)
val resultEquilateral = getTriangleTypes(inputEquilateral._1,inputEquilateral._2, inputEquilateral._3)
val resultError = getTriangleTypes(inputError._1, inputError._2, inputError._3)
//
// Assert
// ------
assert (resultScalene == TriangleTypes.SCALENE)
assert (resultIsosceles == TriangleTypes.ISOSCELES)
assert (resultEquilateral == TriangleTypes.EQUILATERAL)
assert (resultError == TriangleTypes.ERROR)
