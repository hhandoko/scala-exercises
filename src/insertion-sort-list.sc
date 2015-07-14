// Problem:
//   Sort a linked list using insertion sort.
//
//   Definition for a singly-linked list (Java):
//     public class ListNode {
//         int val;
//         ListNode next;
//         ListNode(int x) { val = x; }
//     }
//
// Source: https://leetcode.com/problems/insertion-sort-list/
//
// Definitions
// -----------
case class ListNode(value: Int, var next: Option[ListNode])
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
    val reversed = xs.reverse
    reversed.tail.foldLeft(ListNode(reversed.head)) {
      (acc, item) => new ListNode(item, Some(acc))
    }
  }

}
//
// Input and expected output
// -------------------------
val input = ListNode(Array(6, 5, 3, 1, 8, 7, 2, 4))
val expected = ListNode(Array(1, 2, 3, 4, 5, 6, 7, 8))
//
// Arrange
// -------
/**
 * Sort the singly-linked list.
 *
 * @param head the linked list head node.
 * @return the sorted linked list.
 */
def insertionSortList(head: ListNode): ListNode = {

  // Iterate through the singly-linked list, checking if the next node's value
  // is smaller than the current node.
  // If so, add it as the head of the `sorted` list. Otherwise, continue down
  // the node and insert the node at the appropriate location (between two nodes
  // by updating the pointer) or append it at the end of the pointer.

  var sorted = new ListNode(head.value, None)
  var trail = head.next

  do {
    var pointer = trail.get
    trail = trail.get.next

    if (pointer.value < sorted.value) {
      // Insert at head:
      // Create a new list with `pointer` value as head node, and current `sorted` list as `next` values.
      sorted = ListNode(pointer.value, Some(sorted))
      pointer = sorted
    } else {
      var innerPointer = sorted
      var innerInsert = false
      // Insert at middle:
      // Update `sorted` list by swapping the current `innerPointer` `next` value with `pointer` value,
      // and appending the `pointer` value with the original `innerPointer` `next`.
      //
      // Visual explanation:
      //   Before:
      //     sorted       = 1 -> 3 -> 5 -> 6 -> 7 -> 8
      //     innerPointer = 1 -> 3 -> 5 -> 6 -> 7 -> 8
      //     pointer      = 2 -> 4
      //   Process 1: val tempPointer = innerPointer.next
      //     sorted       = 1 -> 3 -> 5 -> 6 -> 7 -> 8
      //     innerPointer = 1 -> 3 -> 5 -> 6 -> 7 -> 8
      //     pointer      = 2 -> 4
      //     tempPointer  = (3 -> 5 -> 6 -> 7 -> 8)
      //   Process 2: innerPointer.next = Some(pointer)
      //     sorted       = 1 -> 3 -> 5 -> 6 -> 7 -> 8
      //     innerPointer = 1 -> (2 -> 4)
      //     pointer      = 2 -> 4
      //     tempPointer  = 3 -> 5 -> 6 -> 7 -> 8
      //   Process 3: pointer.next = tempPointer
      //     tempPointer  = 3 -> 5 -> 6 -> 7 -> 8
      //                  V
      //     pointer      = 2 -> (3 -> 5 -> 6 -> 7 -> 8)
      //                  V
      //     innerPointer = 1 -> 2 -> (3 -> 5 -> 6 -> 7 -> 8)
      //                  V
      //     sorted       = 1 -> 2 -> (3 -> 5 -> 6 -> 7 -> 8)
      //   After:
      //     sorted       = 1 -> 2 -> 3 -> 5 -> 6 -> 7 -> 8
      //     innerPointer = 1 -> 2 -> 3 -> 5 -> 6 -> 7 -> 8
      //     pointer      = 2 -> 3 -> 5 -> 6 -> 7 -> 8
      do {
        if (pointer.value > innerPointer.value && pointer.value <= innerPointer.next.get.value) {
          val tempPointer = innerPointer.next
          innerPointer.next = Some(pointer)
          pointer.next = tempPointer
          innerInsert = true
        }

        innerPointer = innerPointer.next.get
      } while(innerPointer.next.isDefined && !innerInsert)

      // Insert at end:
      // Update `sorted` end node with `pointer` value.
      if (!innerInsert) {
        innerPointer.next = Some(ListNode(pointer.value))
      }
    }
  } while(trail.nonEmpty)

  sorted

}
//
// Act
// ---
val result = insertionSortList(input)
//
// Assert
// ------
assert (expected == result)