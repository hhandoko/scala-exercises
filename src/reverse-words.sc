// Problem:
//   Write a function to reverse the words in a string, for example `cat and dog` becomes `tac dna god`.
//
//   Please do not use any string manipulation functions that you do not implement yourself.
//
//   Your code:
//     - Should reverse each word within the string without altering whitespace characters
//     - Should treat punctuation characters as part of the word
//     - Is only required to deal with standard English characters. Unicode support is not required.
//     - Should be your own code and should not use any built-in string manipulation calls.
//
// Input and expected output
// -------------------------
val input = "cat and dog"
val expected = "tac dna god"
//
// Arrange
// -------
/**
 * Reverse the words in a sentence.
 *
 * @param sentence the sentence containing the words to reverse.
 * @return the sentence with reversed words.
 */
def reverseWord(sentence: String): String = {

  // Use Scala's collection, `map`, and `mkString` function. Magic!

  val DELIMITER = " "
  sentence.split(DELIMITER).map { word =>
    word.toCharArray.reverse.mkString
  }.mkString(DELIMITER)

}
//
// Act
// ---
val result = reverseWord(input)
//
// Assert
// ------
assert (expected == result)