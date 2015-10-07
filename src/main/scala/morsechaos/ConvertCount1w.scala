package morsechaos

import scala.collection.JavaConverters._
import scala.collection.mutable
import java.nio.file._

object ConvertCount1w {
  def main(args: Array[String]): Unit = {
    val lines1 = Files.readAllLines(Paths get "data/count_1w.txt").asScala
    val split1 = lines1.map { l => val sp = l split '\t'; sp(0) -> sp(1).toLong }
    val split2 = split1.map { case (word, count) => word.toLowerCase -> count }
    val split3 = distinctBy(split2, (t: (String, Long)) => t._1)
    val split4 = split3.filter { case (word, _) => !Set("i", "a").contains(word) && word.length > 2 }
    val split5 = split4.filter { case (word, _) => word.forall(ch => Morse.charToMorse.keySet.contains(ch)) }
    val split6 = split5.map { case (word, count) => (word.flatMap(Morse.encode(_)), word, count) }
    Files.write(Paths get "data/count_1w.morse.txt",
      split6.map { case (morse, word, count) => s"$morse $word $count" }.asJava)
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
