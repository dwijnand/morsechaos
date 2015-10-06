package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

/** Convert http://www-personal.umich.edu/~jlawler/wordlist.html */
object ConvertWordlist {
  def main(args: Array[String]): Unit = {
    val lines1 = Files.readAllLines(Paths.get("data/wordlist.txt")).asScala
    val lines2 = lines1.map(_.toLowerCase).distinct
    val lines3 = lines2.filter(_.length > 2) ++ Seq("i", "a")
    val lines4 = lines3.filter(_.forall(ch => Morse.charToMorse.keySet.contains(ch)))
    val lines5 = lines4.map(line => line.flatMap(ch => Morse.encode(ch)) -> line)
    Files.write(Paths.get("data/wordlist.morse.txt"), lines5.map { case (morse, line) => s"$morse $line" }.asJava)
  }
}
