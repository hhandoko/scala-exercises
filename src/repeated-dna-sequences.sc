// https://leetcode.com/problems/repeated-dna-sequences/
// Input and expected output
val input = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"
val expected = Array("AAAAACCCCC", "CCCCCAAAAA")

// Arrange
// -------
val seqLimit = 10
var seqs = Array[String]()
for (i <- 0 to input.length - seqLimit) {
  seqs :+= input.substring(i, i + seqLimit)
}

// Act
// ---
val result = seqs.groupBy(identity).collect {
  case (x, ys) if ys.length > 1 => x
}.toArray.sorted

// Assert
// ------
assert (expected.sameElements(result))
assert (expected.deep == result.deep)