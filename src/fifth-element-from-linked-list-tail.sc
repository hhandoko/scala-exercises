// Problem:
//   Write a function that would return the 5th element from the tail (or end) of a singly linked list of integers.
//   For example, given the list 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 10 -> 11 your function would return 7.
//
//   Your code
//     - Should return the 5th element from the tail in a single pass.
//     - Should be your own code and should not use any built-in collection components.
//
//   Definition for a singly-linked list (Java):
//     public class ListNode {
//         int val;
//         ListNode next;
//         ListNode(int x) { val = x; }
//     }
import scala.annotation.tailrec
//
// Definitions
// -----------
case class ListNode(value: Int, next: Option[ListNode])
object ListNode {

  /**
   * Initialise an instance of `ListNode` given only its value.
   *
   * @param x the `ListNode` value.
   * @return the instance.
   */
  def apply(x: Int): ListNode = this(x, None)

  /**
   * Initialise an instance of `ListNode` given an array of values.
   *
   * @param xs the `ListNode` values.
   * @return the instance.
   */
  def apply(xs: Array[Int]): ListNode = {
    xs.init.foldRight(ListNode(xs.last)) { (i, acc) => ListNode(i, Some(acc)) }
  }

}
//
// Input and expected output
// -------------------------
val input = ListNode(Array(2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
val expectedParamNthFromTail = 5
val expected = 7
//
// Arrange
// -------
/**
 * Gets the nth node from tail.
 *
 * @param head the linked list head node.
 * @param indexFromTail the nth index from tail.
 * @return the nth node from tail
 */
def getNthFromTail(head: ListNode, indexFromTail: Int): ListNode = {

  /**
   * Tail-recurse to advance the node to get the nth node.
   *
   * Use two pointers. Advance the shadow node to nth index (param),
   * and then start advancing the node. When the shadow node gets to the end
   * of the linked list, the current node should contain the value we're
   * looking for.
   *
   * Visual explanation:
   *   Start index:
   *     shadow = (2), 3, 4, 5, 6, 7, 8, 9, 10, 11
   *     node   = (2), 3, 4, 5, 6, 7, 8, 9, 10, 11
   *   5th index:
   *     shadow = 2, 3, 4, 5, (6), 7, 8, 9, 10, 11
   *     node   = (2), 3, 4, 5, 6, 7, 8, 9, 10, 11
   *   End index:
   *     shadow = 2, 3, 4, 5, 6, 7, 8, 9, 10, (11)
   *     node   = 2, 3, 4, 5, 6, (7), 8, 9, 10, 11
   *
   * @param node the linked list head node.
   * @param shadow the linked list shadow node.
   * @param counter the counter.
   * @return the nth node from tail.
   */
  @tailrec
  def advance(node: ListNode, shadow: ListNode, counter: Int): ListNode = {
    if (shadow.next.isDefined && counter > 0) advance(node, shadow.next.get, counter - 1)
    else if (shadow.next.isDefined) advance(node.next.get, shadow.next.get, 0)
    else node
  }

  advance(head, head, indexFromTail - 1)

}
//
// Act
// ---
val result = getNthFromTail(input, expectedParamNthFromTail).value
//
// Assert
// ------
assert (expected == result)