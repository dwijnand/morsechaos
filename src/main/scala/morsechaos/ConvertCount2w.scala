package morsechaos

import java.nio.file._

import scala.collection.JavaConverters._
import scala.collection.mutable

object ConvertCount2w {
  def main(args: Array[String]): Unit = {
    val lines1 = Files.readAllLines(Paths get "data/count_2w.txt").asScala
    val split1 = lines1.map { l => val sp = l split '\t'; val sp2 = sp(0) split ' '; (sp2(0), sp2(1), sp(1).toLong) }
    val split2 = split1.map { case (w1, w2, count) => (w1.toLowerCase, w2.toLowerCase, count) }
    val split3 = distinctBy(split2, (t: (String, String, Long)) => (t._1, t._2))
    val split4 = split3.filter { case (w1, _, _) => !Set("i", "a").contains(w1) && w1.length > 1 }
    val split5 = split4.filter { case (_, w2, _) => !Set("i", "a").contains(w2) && w2.length > 1 }
    val split6 = split5.filter { case (w1, _, _) => w1.forall(ch => Morse.charToMorse.keySet.contains(ch)) }
    val split7 = split6.filter { case (_, w2, _) => w2.forall(ch => Morse.charToMorse.keySet.contains(ch)) }
    val split8 = split7.map { case (w1, w2, count) =>
      (w1.flatMap(Morse.encode(_)), w2.flatMap(Morse.encode(_)), w1, w2, count) }
    Files.write(Paths get "data/count_2w.morse.txt",
      split8.map { case (m1, m2, w1, w2, count) => s"$m1 $m2 $w1 $w2 $count" }.asJava)
  }

  def distinctBy[A, B](coll: Seq[A], f: A => B): Seq[A] = {
    val b = Seq.newBuilder[A]
    val seen = mutable.HashSet[B]()
    for (x <- coll) {
      val fx = f(x)
      if (!seen(fx)) {
        b += x
        seen += fx
      }
    }
    b.result()
  }
}
