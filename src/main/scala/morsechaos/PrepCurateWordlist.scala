package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object PrepCurateWordlist {
  def main(args: Array[String]): Unit = {
    val lines1 = Files.readAllLines(Paths.get("data/wordlist.morse.txt")).asScala
    val lines2 = lines1.map { l => val a = l.split(' '); a(0) -> a(1) }
    val lines3 = lines2.sortBy(_._1.length)
    Files.write(Paths.get("data/wordlist.morse.curated.txt"),
      lines3.map { case (morse, line) => s"$morse $line" }.asJava)
  }
}
