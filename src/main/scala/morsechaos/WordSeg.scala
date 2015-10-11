package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

// Based on http://jeremykun.com/2012/01/15/word-segmentation/

object WordSeg {
  def splitPairs(word: String): Seq[(String, String)] =
    0.until(word.length).map(i => word splitAt i + 1)

  val segment: Memo1[String, Seq[String]] = Memo1((word: String) =>
    if (word.isEmpty) Vector.empty
    else {
      val allSegmentations =
        splitPairs(word).map { case (first, rest) => Vector(first) ++ segment(rest) }
      allSegmentations maxBy wordSeqFitness
    }
  )

  val singleWordProb = OneGramDist(Paths get "data/count_1w.txt")

  def wordSeqFitness(words: Seq[String]): Double = words.map(w => math.log10(singleWordProb(w))).sum
}

class OneGramDist(val dict: Map[String, Long], val gramCount: Long) {
  def apply(word: String): Double =
    if (dict contains word)
      dict(word).toDouble / gramCount
    else
      1.0 / (gramCount * (if (word.length < 3) 1.0 else math.pow(10, 1.4 * (word.length - 3))))
}

object OneGramDist {
  def apply(p: Path) = {
    val dict = Files.readAllLines(p).asScala.map { l => val sp = l split '\t'; sp(0) -> sp(1).toLong }.toMap
    val gramCount = dict.values.sum
    new OneGramDist(dict, gramCount)
  }
}
