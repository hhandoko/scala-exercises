// Problem:
//   All DNA is composed of a series of nucleotides abbreviated as A, C, G, and T, for example: "ACGAATTCCG". When studying DNA, it is sometimes useful to identify repeated sequences within the DNA.
//   Write a function to find all the 10-letter-long sequences (substrings) that occur more than once in a DNA molecule.
//
//   For example,
//     Given s = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT",
//
//   Return:
//     ["AAAAACCCCC", "CCCCCAAAAA"].
//
// Source: https://leetcode.com/problems/repeated-dna-sequences/
//
// Input and expected output
// -------------------------
val input = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"
val expected = Array("AAAAACCCCC", "CCCCCAAAAA")
//
// Arrange
// -------
/**
 * Find 10-char repeated sequence in a sequence of string.
 *
 * @param s the string sequence to evaluate.
 * @return the array of 10-char repeated sequence.
 */
def findRepeatedDnaSequences(s: String): Array[String] = {

  // Split the input string into array of 10-char sequences, then run Scala's collection
  // `groupBy` to filter out elements with duplicates.

  val seqLimit = 10
  var seqs = Array[String]()

  // Split into 10-char sequence array
  for (i <- 0 to input.length - seqLimit) {
    seqs :+= input.substring(i, i + seqLimit)
  }

  // Use `groupBy` to detect and extract duplicates
  seqs.groupBy(identity).collect {
    case (x, ys) if ys.length > 1 => x
  }.toArray.sorted

}
//
// Act
// ---
val result = findRepeatedDnaSequences(input)
//
// Assert
// ------
assert (expected.sameElements(result))
assert (expected.deep == result.deep)