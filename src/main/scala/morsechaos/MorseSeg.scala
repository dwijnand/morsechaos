package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object MorseSeg {
  def main(args: Array[String]): Unit = println(segmentDecode(args.head))

  def splitPairs(word: String): Seq[(String, String)] = {
    println(s"Splitting word of ${word.length} characters")
    0.until(word.length).map(i => word splitAt i + 1)
  }

  val segment: Memo1[String, Seq[String]] = Memo1((word: String) =>
    if (word.isEmpty) Vector.empty
    else {
      val allSegmentations =
        splitPairs(word).map { case (first, rest) => Vector(first) ++ segment(rest) }
      allSegmentations maxBy wordSeqFitness
    }
  )

  val singleWordProb = OneGramMorseDist(Paths get "data/count_1w.morse.txt")

  def segmentDecode(word: String) =
    segment(word).map(w => singleWordProb.dict.get(w).map(_.maxBy(_._2)._1).getOrElse(w)) mkString " "

  def wordSeqFitness(words: Seq[String]): Double =
    words.map { w => math.log10(singleWordProb(w)) }.sum
}

class OneGramMorseDist(val dict: Map[String, Vector[(String, Long)]], val gramCount: Long) {
  def apply(word: String): Double =
    if (dict contains word)
      dict(word).map(_._2.toDouble / gramCount).max
    else
      probForUnknownWord(word)

  // todo kill
  def apply2(word: String): Vector[(Option[String], Double)] =
    if (dict contains word) {
      val dict1: Vector[(String, Long)] = dict(word)
      dict1.map { case (w, prob) => (Some(w), prob.toDouble / gramCount) }
    } else
      Vector(None -> probForUnknownWord(word))

  private def probForUnknownWord(word: String): Double = {
    1.0 / (gramCount * math.pow(10, (word.length - 9) / 3))
  }
}

object OneGramMorseDist {
  def apply(p: Path) = {
    val lines = Files.readAllLines(p).asScala
    val dict = lines
      .map { l => val sp = l split ' '; (sp(0), sp(1), sp(2).toLong) }
      .groupBy(_._1)
      .map { case (morse, values) => morse -> values.map { case (_, word, count) => (word, count) }.toVector }
    val gramCount = dict.values.flatMap(_.map(_._2)).sum
    new OneGramMorseDist(dict, gramCount)
  }
}
