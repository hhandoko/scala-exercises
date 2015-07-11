// https://leetcode.com/problems/repeated-dna-sequences/

// Input and expected output
val input = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"
val expected = Array("AAAAACCCCC", "CCCCCAAAAA")

// Arrange
// -------
def findRepeatedDnaSequences(s: String): Array[String] = {

  val seqLimit = 10
  var seqs = Array[String]()
  for (i <- 0 to input.length - seqLimit) {
    seqs :+= input.substring(i, i + seqLimit)
  }

  seqs.groupBy(identity).collect {
    case (x, ys) if ys.length > 1 => x
  }.toArray.sorted

}

// Act
// ---
val result = findRepeatedDnaSequences(input)

// Assert
// ------
assert (expected.sameElements(result))
assert (expected.deep == result.deep)