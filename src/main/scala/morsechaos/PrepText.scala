package morsechaos

import scala.collection.JavaConverters._
import java.nio.charset._
import java.nio.file._

object PrepText {
  def main(args: Array[String]): Unit = {
    val oneString = Files.readAllLines(Paths.get("signs.txt"), Charset.forName("ISO-8859-1")).asScala.mkString
    val fixedString = oneString.map { case '-' => '-' ; case 183 => '.' }
    Files.write(Paths.get("signs.clean.txt"), fixedString.grouped(118).toSeq.asJava)
  }
}
