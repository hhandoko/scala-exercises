// Problem:
//   Sort a linked list using insertion sort.
//
//   Definition for a singly-linked list (Java):
//      public class ListNode {
//          int val;
//          ListNode next;
//          ListNode(int x) { val = x; }
//      }
//
// Source: https://leetcode.com/problems/insertion-sort-list/
//
// Definitions
// -----------
case class ListNode(value: Int, var next: Option[ListNode])
object ListNode {
  def apply(x: Int): ListNode = this(x, None)
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
def insertionSortList(head: ListNode): ListNode = {

  var sorted = new ListNode(head.value, None)
  var node = head.next

  do {
    var pointer = node.get
    node = node.get.next

    if (pointer.value < sorted.value) {
      // Insert at head
      sorted = ListNode(pointer.value, Some(sorted))
      pointer = sorted
    } else {
      var innerPointer = sorted
      var innerInsert = false
      // Insert at middle
      do {
        if (pointer.value > innerPointer.value && pointer.value <= innerPointer.next.get.value) {
          val tempPointer = innerPointer.next
          innerPointer.next = Some(pointer)
          pointer.next = tempPointer
          innerInsert = true
        }

        innerPointer = innerPointer.next.get
      } while(innerPointer.next.isDefined && !innerInsert)

      // Insert at end
      if (!innerInsert) {
        innerPointer.next = Some(ListNode(pointer.value))
      }
    }
  } while(node.nonEmpty)

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