package morsechaos

import scala.collection.JavaConverters._
import scala.math._
import java.nio.file._

object MorseSeg {
  def main(args: Array[String]): Unit = segmentBoth(args.head)

  def encode(english: String) =
    english.filter(_ != ' ').filter(_ != '\n') flatMap {
      case '.' => "."
      case '-' => "-"
      case ch  => Morse encode ch
    }

  def resegment2All(english: String) = segment2AllDecode(encode(english))

  def resegmentAll(english: String) = segmentAllDecode(encode(english))

  def resegment(english: String, prev: String = "<S>") = segmentBoth(encode(english), prev)

  def segmentAllDecode(word: String) = segmentAll(word) foreach { case (ws, _) => println(ws mkString " ") }

  def segment2AllDecode(word: String, prev: String = "<S>") =
    segment2All((word, prev)) foreach { case (_, ws) => println(ws mkString " ") }

  def segmentBoth(word: String, prev: String = "<S>") = {
    println(segment2Decode(word, prev))
    println(segmentDecode(word))
  }

  def segmentDecode(word: String) = segment(word) map maxDecode mkString " "

  def segment2Decode(word: String): String = segment2Decode(word, "<S>")
  def segment2Decode(word: String, prev: String) = segment2((word, prev, true))._2 map maxDecode mkString " "

  def maxDecode(w: String) = singleWordProb.dict.get(w).map(_.maxBy(_._2)._1) getOrElse w

  def splitDecode(morse: String): Iterator[String] = {
    if (morse.length == 0) Iterator("")
    else {
      (1 to (4 min morse.length)).iterator.flatMap { i =>
        val (l, r) = morse splitAt i
        Morse decodeOpt l match {
          case Some(ch) => splitDecode(r) map (s => ch.toString + s)
          case None     => Iterator.empty
        }
      }
    }
  }

  def splitPairs(word: String): Seq[(String, String)] = 0.until(word.length).map(i => word splitAt i + 1)

  val singleWordProb = OneGramMorseDist(Paths get "data/count_1w.morse.txt")
  val twoWordProb = TwoGramMorseDict(Paths get "data/count_2w.morse.txt")

  val segment: Memo1[String, Seq[String]] = Memo1((word: String) =>
    if (word.isEmpty) Vector.empty
    else {
      val allSegmentations = splitPairs(word).map { case (first, rest) => first +: segment(rest) }
      allSegmentations maxBy wordSeqFitness
    }
  )

  def wordSeqFitness(words: Seq[String]): Double =
    words.map { w => log10(singleWordProb(w)) }.sum

  val segmentAll: Memo1[String, Seq[(Vector[String], Double)]] = Memo1((word: String) =>
    if (word.isEmpty) Vector((Vector.empty, 0.0))
    else
      splitPairs(word).flatMap { case (first, rest) =>
        singleWordProb.getAll(first)
          .map { case (w, p) => w -> log10(p) }
          .flatMap { case (w, p) =>
            segmentAll(rest) map { case (ws, ps) => (w +: ws) -> (p + ps) }
          }
      }.sortBy(_._2).takeRight(30)
  )

  def condProbOfWord(word: String, prev: String): Double = {
    val prob2 = twoWordProb get s"$prev $word"
    val probPrev = singleWordProb get prev
    val ret = (prob2, probPrev) match {
      case (Some(prob2), Some(probPrev)) => if (probPrev == 0.0) prob2 else prob2 / probPrev
      case _                             => singleWordProb(word)
    }
    ret
  }

  def condProbOfWordAll(word: String, prev: String): Vector[(String, Double)] = {
    val probs = twoWordProb getAll s"$prev $word"
    val probsPrev = singleWordProb getAll prev
    val ret = (probs, probsPrev) match {
      case (Vector(), Vector()) => singleWordProb getAll word
      case (probs, probsPrev)    =>
        for {
          (_, probPrev) <- probsPrev
          (english, prob) <- probs
        } yield if (probPrev == 0.0) (english, prob) else (english, prob / probPrev)
    }
    ret
  }

  val segment2: Memo1[(String, String, Boolean), (Double, Seq[String])] = Memo1 { wordAndPrev: (String, String, Boolean) =>
    val (word, prev, print) = wordAndPrev
    if (word.isEmpty) {
      (0.0, Vector.empty)
    } else {
      val candidates =
        splitPairs(word).map { case (first, rem) =>
          val condProb = log10(condProbOfWord(first, prev))
          val (probRem, rem2) = segment2((rem, first, false))
          (condProb + probRem, first +: rem2)
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

  val segment2All: Memo1[(String, String), Seq[(Double, Seq[String])]] = Memo1 { wordAndPrev: (String, String) =>
    val (word, prev) = wordAndPrev
    if (word.isEmpty) {
      Vector((0.0, Vector.empty))
    } else {
      (for {
        (first, rem) <- splitPairs(word)
        (probRem, rem2) <- segment2All((rem, first))
        (first2, prob) <- condProbOfWordAll(first, prev).map { case (eng, prob) => (eng, log10(prob)) }
      } yield
        (prob + probRem, first2 +: rem2)
      ).sortBy(_._1).takeRight(30)
    }
  }
}

class OneGramMorseDist(val dict: Map[String, Vector[(String, Long)]], val gramCount: Long) {
  def getAll(word: String): Vector[(String, Double)] =
    dict.getOrElse(word, Vector.empty) map { case (w, p) => w -> p.toDouble / gramCount }

  def get(word: String): Option[Double] =
    getAll(word) match { case Vector() => None ; case xs => Some(xs.map(_._2).max) }

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
