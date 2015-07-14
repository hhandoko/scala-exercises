// Problem:
//   Given preorder and inorder traversal of a tree, construct the binary tree.
//
//   Note: You may assume that duplicates do not exist in the tree.
//
//   Definition for a binary tree node (Java):
//     public class TreeNode {
//         int val;
//         TreeNode left;
//         TreeNode right;
//         TreeNode(int x) { val = x; }
//     }
//
// Source: https://leetcode.com/problems/construct-binary-tree-from-preorder-and-inorder-traversal/
// Hints : http://articles.leetcode.com/2011/04/construct-binary-tree-from-inorder-and-preorder-postorder-traversal.html
//
// Definitions
// -----------
case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])
object TreeNode {

  /**
   * Create an instance of `TreeNode`
   *
   * @param value the `TreeNode` value.
   * @return the instance.
   */
  def apply(value: Int): TreeNode = this(value, None, None)

}
//
// Input and expected output
// -------------------------
val inputPreorder = Array(7, 10, 4, 3, 1, 2, 8, 11)
val inputInorder = Array(4, 10, 3, 1, 7, 11, 8, 2)
