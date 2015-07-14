// Problem:
//   A squad of robotic explorers are to be landed on the planet Venus.
//
//   The landing area is rectangular and must be navigated by the explorers so that their on-board cameras can get a
//   complete view of the surrounding terrain to send back to Earth.
//
//   An explorer's position and location is represented by a combination of `x` and `y` co-ordinates and a letter
//   representing one of the four cardinal compass points. The landing area is divided up into a grid to simplify
//   navigation. An example position might be `0`, `0`, `N`, which means the explorer is in the bottom left corner and
//   facing North.
//
//   In order to control an explorer, we send a simple string of letters. The possible letters are `L`, `R` and `M`.
//   `L` and `R` makes the explorer spin 90 degrees left or right respectively, without moving from its current spot.
//   `M` means move forward one grid point, and maintain the same heading.
//
//   Assume that the square directly North from `(x, y)` is `(x, y + 1)`.
//
// Input:
//   The first line of input is the upper-right coordinates of the landing area, the lower-left coordinates are assumed
//   to be `0, 0`.
//
//   The rest of the input is information pertaining to the explorers that have been deployed. Each explorer has two
//   lines of input. The first line gives the explorer’s position, and the second line is a series of instructions
//   telling the explorer how to explore the plateau.
//
//   The position is made up of two integers and a letter separated by spaces, corresponding to the `x` and `y`
//   co-ordinates and the explorer’s orientation.
//
//   Each explorer will be finished sequentially, which means that the second explorer won’t start to move until the
//   first one has finished moving.
import scala.collection.mutable.ArrayBuffer
//
// Definitions
// -----------
//
// Direction enumeration
sealed trait Direction { def name: String; def shortName: String; def d: Int }
object Direction {
  case object N extends Direction { val name = "North"; val shortName = "N"; val d = 0 }
  case object E extends Direction { val name = "East"; val shortName = "E"; val d = 90 }
  case object S extends Direction { val name = "South"; val shortName = "S"; val d = 180 }
  case object W extends Direction { val name = "West"; val shortName = "W"; val d = 270 }

  val findAll = Seq(N, E, S, W)
}
//
// Robot object
sealed trait RobotCommands {

  /**
   * The X coordinate on the map.
   */
  var x: Int

  /**
   * The Y coordinate on the map.
   */
  var y: Int

  /**
   * The current heading.
   */
  var heading: Direction

  /**
   * The map upper right boundary.
   */
  val boundary: (Int, Int)

  /**
   * Get the current coordinate.
   *
   * @return the current coordinate as string.
   */
  def getCurrentCoordinate: String = {
    s"$x $y ${heading.shortName}"
  }

  /**
   * Pass a command to the robot.
   *
   * @param commands the commands.
   */
  def command(commands: String): Unit = {
    commands.toCharArray.foreach(command)
  }

  /**
   * Pass a command to the robot.
   *
   * @param commands the commands.
   */
  def command(commands: Char): Unit = {
    commands match {
      case t if t == 'L' || t == 'R' => {
        t match {
          case 'L' => turnLeft()
          case 'R' => turnRight()
        }
      }
      case m if m == 'M' => moveForward()
    }
  }

  /**
   * Command the robot to turn left.
   */
  private def turnLeft(): Unit = {
    heading = heading match {
      case Direction.N => Direction.W
      case _ => Direction.findAll.filter(q => q.d == heading.d - 90).head
    }
  }

  /**
   * Command the robot to turn right.
   */
  private def turnRight(): Unit = {
    heading = heading match {
      case Direction.W => Direction.N
      case _ => Direction.findAll.filter(q => q.d == heading.d + 90).head
    }
  }

  /**
   * Command the robot to move forward.
   *
   * Note: We assume that individual robot can occupy the same space in the map (i.e. no collision).
   */
  private def moveForward(): Unit = {
    heading match {
      // Move robot if only is still within the map boundary
      case d if d == Direction.N && y < boundary._2 => y = y + 1
      case d if d == Direction.E && x < boundary._1 => x = x + 1
      case d if d == Direction.S && y > 0 => y = y - 1
      case d if d == Direction.W && x > 0 => x = x - 1
    }
  }

}
case class Robot(var x: Int, var y: Int, var heading: Direction, boundary: (Int, Int)) extends RobotCommands
object Robot {

  /**
   * Implicitly maps string to Direction enum.
   *
   * @param s the string to map.
   * @return the Direction.
   */
  implicit def stringToDirection(s: String): Direction = {
    Direction.findAll.filter(q => q.shortName == s).head
  }

  /**
   * Initialise an instance of `Robot`.
   *
   * @param init the initialisation parameters.
   * @param boundary the map boundary.
   * @return the instance.
   */
  def apply(init: String, boundary: (Int, Int)): Robot = {
    val params = init.split(" ")
    new Robot(params(0).toInt, params(1).toInt, params(2), boundary)
  }

}
//
// Input and expected output
// -------------------------
val input =
  """
   |5 5
   |1 2 N
   |LMLMLMLMM
   |3 3 E
   |MMRMMRMRRM
  """.stripMargin.trim
val expected =
  """
   |1 3 N
   |5 1 E
  """.stripMargin.trim
//
// Arrange
// -------
/**
 * Get the robot explorer(s) final coordinates given a set of command(s).
 *
 * @param commands the command(s)
 * @return
 */
def getFinalCoordinates(commands: String): Array[String] = {

  /**
   * Implicitly maps a string into a binary Int tuple.
   *
   * @param s the string to map.
   * @return the binary Int tuple.
   */
  implicit def stringToBinaryTuple(s: String): (Int, Int) = {
    val values = s.trim.split(" ")
    (values(0).toInt, values(1).toInt)
  }

  // Parse the (multi-line) commands into a sequence of command
  val sequence = commands.split("[\r?\n]+").map(_.trim).filter(q => q.length > 0)

  // First sequence is always the map boundary
  val boundary: (Int, Int) = sequence(0)

  // Find how many robots are expected to be commanded
  // Note: Assuming each robot will take two sequence of commands
  val totalRobots = (sequence.length - 1) / 2

  // Create robots references, and initialise their position
  val robots = ArrayBuffer[Robot]()
  for(i <- 0 until totalRobots) {
    robots += Robot(sequence((i * 2) + 1), boundary)
  }

  // Give out commands to each of the robots (sequentially)
  for(i <- 0 until totalRobots) {
    robots(i).command(sequence((i + 1) * 2))
  }

  // Get final coordinates report from each robot
  robots.map(_.getCurrentCoordinate).toArray

}
//
// Act
// ---
val result = getFinalCoordinates(input).mkString("\r\n")
//
// Assert
// ------
assert (expected == result)