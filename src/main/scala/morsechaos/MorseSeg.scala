package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object MorseSeg {
  def main(args: Array[String]): Unit = println(segmentDecode(args.head))

  def segmentDecode(word: String) = segment(word) map maxDecode mkString " "

  def maxDecode(w: String) = singleWordProb.dict.get(w).map(_.maxBy(_._2)._1) getOrElse w

  def splitPairs(word: String): Seq[(String, String)] = 0.until(word.length).map(i => word splitAt i + 1)

  val segment: Memo1[String, Seq[String]] = Memo1((word: String) =>
    if (word.isEmpty) Vector.empty
    else {
      val allSegmentations = splitPairs(word).map { case (first, rest) => first +: segment(rest) }
      allSegmentations maxBy wordSeqFitness
    }
  )

  val singleWordProb = OneGramMorseDist(Paths get "data/count_1w.morse.txt")

  def wordSeqFitness(words: Seq[String]): Double =
    words.map { w => math.log10(singleWordProb(w)) }.sum
}

class OneGramMorseDist(val dict: Map[String, Vector[(String, Long)]], val gramCount: Long) {
  def get(word: String): Option[Double] = dict.get(word).map(_.map(_._2).max.toDouble / gramCount)

  def apply(word: String): Double = get(word) getOrElse probForUnknownWord(word)

  private def probForUnknownWord(word: String): Double = 1.0 / gramCount * probForUnknownWordLen(word.length)

  private def probForUnknownWordLen(l: Int): Double =
    if (l < 3) 0.0 else if (l < 9) 1.0 else 1.0 / math.pow(10, 0.3 * (l - 9).toDouble)
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

object TwoGramMorseDict {
  def apply(p: Path) = {
    val lines = Files.readAllLines(p).asScala
    val dict = lines
      .map { l => val sp = l split ' '; (s"${sp(0)} ${sp(1)}", s"${sp(2)} ${sp(3)}", sp(4).toLong) }
      .groupBy(_._1)
      .map { case (morse, values) => morse -> values.map { case (_, word, count) => (word, count) }.toVector }
    val gramCount = dict.values.flatMap(_.map(_._2)).sum
    new OneGramMorseDist(dict, gramCount)
  }
}
