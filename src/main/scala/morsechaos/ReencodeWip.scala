package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object ReencodeWip {
  def main(args: Array[String]): Unit = {
    val text1 = Files.readAllLines(Paths.get("wip.txt")).asScala.mkString
    val text2 = text1.filter(_ != ' ')
    val text3 = text2.flatMap {
      case '.' => "."
      case '-' => "-"
      case ch  => Morse encode ch
    }
    Files.write(Paths.get("wip.re_encoded.txt"), text3.grouped(118).toSeq.asJava)
    ()
  }
}
