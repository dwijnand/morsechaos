package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object MorseSeg {
  def main(args: Array[String]): Unit = println(segmentDecode(args.head))

  def segmentDecode(word: String) = segment(word) map maxDecode mkString " "

  def segment2Decode(word: String): String = segment2Decode(word, "<S>")
  def segment2Decode(word: String, prev: String) = segment2((word, prev, true))._2 map maxDecode mkString " "

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
  val twoWordProb = TwoGramMorseDict(Paths get "data/count_2w.morse.txt")

  def wordSeqFitness(words: Seq[String]): Double =
    words.map { w => math.log10(singleWordProb(w)) }.sum

  def condProbOfWord(word: String, prev: String): Double = {
    val prob2 = twoWordProb get s"$prev $word"
    val probPrev = singleWordProb get prev
//    println(s"Getting cond prob of [$word] with prev [$prev]")
//    println(s"prob of [$prev $word]: $prob2")
//    println(s"prob of prev [$prev]: $probPrev")
    val ret = (prob2, probPrev) match {
      case (Some(prob2), Some(probPrev)) => if (probPrev == 0.0) prob2 else prob2 / probPrev
      case _                             => singleWordProb(word)
    }
//    println(s"Returning $ret")
//    println()
    ret
  }

  val segment2: Memo1[(String, String, Boolean), (Double, Seq[String])] = Memo1 { wordAndPrev: (String, String, Boolean) =>
    val (word, prev, print) = wordAndPrev
//    println(s"Segmenting [$word] with prev [$prev]")
    if (word.isEmpty) {
//      println(s"word is empty, returning (0.0 Vector.empty)")
      (0.0, Vector.empty)
    }
    else {
//      println(s"Splitting [$word] with prev [$prev]")
      val candidates =
        splitPairs(word).map { case (first, rem) =>
//          println(s"Working on split [$first] + [$rem], with prev [$prev]")
          val (probRem, rem2) = segment2((rem, first, false))
//          println(s"Back from segmenting [$rem] with prev [$first], yields $probRem and $rem2")
//          println(s"Note my prev is [$prev]")
//          println(s"Calculating cond prob of [$first] with prev [$prev]")
          val condProb = condProbOfWord(first, prev)
//          println(s"Cond prob of [$first] with prev [$prev] is $condProb")
          val condProbLogged = math.log10(condProb)
//          println(s"which is $condProbLogged after log10")
          val sumProb = condProbLogged + probRem
          val candidate = first +: rem2
//          println(s"which brings the sum prob of $candidate to $sumProb")
//          println()
          (sumProb, candidate)
        }
      if (print) {
        val sorted = candidates.sortBy(-_._1)
        println(s"Done segmenting [$word] with prev [$prev]")
        println(s"${candidates.size} candidates:")
        sorted foreach { case (prob, segs) => println(s"$prob ${segs map maxDecode mkString " "}") }
      }
      candidates.maxBy(_._1)
    }
  }
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
