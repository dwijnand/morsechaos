package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object SolveLast {
  val prefix = "did not try"

  def main(args: Array[String]): Unit = {
    val text =
      """
        |-.-.-----.--..-...-..----.--...-..--..----.....--.-..-......-.--.....-....-.-.-.....--.--..-...-....-..
        |....-.-.-..-......-.---..---.......-.---.-.-.....-------.-.-....-.-.-----..-.-...--..-.........-.-----.
        |.--.-..--..-...-.---.....---.---........----.-.-.---.-.----.-..-...-..........---.--..-.-----..-.-..---
        |----.--.-..-.........-.-......-.-...-...-.--..
      """.stripMargin.trim.filter(_ != '\n')
    val morseWords = Files.readAllLines(Paths.get("data/wordlist.morse.curated.txt")).asScala
    val dict = morseWords
      .map { s => val a = s.split(' '); a(0) -> a(1) }
      .groupBy(_._1)
      .map { case (k, vs) => k -> vs.map(_._2).toVector }
    val max = dict.keySet.maxBy(_.length).length

    val b1sorted = bob(text, dict, max, Vector(Vector.empty -> 0))
    val b2sorted = bob(text, dict, max, b1sorted)
    val b3sorted = bob(text, dict, max, b2sorted)
    val b4sorted = bob(text, dict, max, b3sorted)
    val b5sorted = bob(text, dict, max, b4sorted)
    val b6sorted = bob(text, dict, max, b5sorted)
    val b7sorted = bob(text, dict, max, b6sorted)

    Files.write(Paths.get("wip.last.5.txt"),
      b7sorted.map { case (ws, _) => s"$prefix ${ws mkString " "}" }.asJava)
    ()
  }

  def bob(text: String, dict: Map[String, Vector[String]], max: Int, input: Vector[(Vector[String], Int)])
  : Vector[(Vector[String], Int)]
  = {
    val b = Vector.newBuilder[(Vector[String], Int)]

    input foreach { case (words0, len0) =>
      1 to max foreach { len =>
        val newLen = len0 + len
        val slice = text.slice(len0, newLen)
        val words = dict get slice
        words.foreach { ws =>
          ws foreach (w => b += (words0 :+ w) -> newLen)
          ws foreach (w => println(s"$prefix ${(words0 :+ w) mkString " "}"))
        }
      }
    }

    b.result().sortBy(_._2)
  }
}
