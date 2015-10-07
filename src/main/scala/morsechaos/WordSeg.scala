package morsechaos

import scala.collection.JavaConverters._
import scala.collection.mutable
import java.nio.file._

// Based on http://jeremykun.com/2012/01/15/word-segmentation/

case class Memo1[T1, R](f: T1 => R) extends (T1 => R) {
  private val cache = mutable.Map.empty[T1, R]
  def apply(v1: T1) = cache.getOrElseUpdate(v1, f(v1))
}

case class Memo2[T1, T2, R](f: (T1, T2) => R) extends ((T1, T2) => R) {
  private val cache = mutable.Map.empty[(T1, T2), R]
  def apply(v1: T1, v2: T2) = cache.getOrElseUpdate((v1, v2), f(v1, v2))
}

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

  def wordSeqFitness(words: Seq[String]) = words.map(w => math.log10(singleWordProb(w))).sum
}

class OneGramDist(val dict: Map[String, Long], val gramCount: Long) {
  def apply(word: String) =
    if (dict contains word)
      dict(word).toDouble / gramCount
    else
      1.0 / (gramCount * math.pow(10, word.length - 1))
}

object OneGramDist {
  def apply(p: Path) = {
    val dict = Files.readAllLines(p).asScala.map { l => val sp = l split '\t'; sp(0) -> sp(1).toLong }.toMap
    val gramCount = dict.values.sum
    new OneGramDist(dict, gramCount)
  }
}
