package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object ConvertDict {
  def main(args: Array[String]): Unit = {
    val lines1 = Files.readAllLines(Paths.get("/usr/share/dict/words")).asScala
    val lines2 = lines1.map(_.toUpperCase).distinct
    val lines3 = lines2.filter(_.length > 2) ++ Seq("I", "A")
    val lines4 = lines3.flatMap { line =>
      val letters = line.toVector map Letter.fromChar
      if (letters.contains(None)) None else Some(letters.flatten)
    }
    val lines5 = lines4.map(line => line.flatMap(Morse.encodeLetter) -> line)
    Files.write(Paths.get("data/dict.morse.txt"),
      lines5.map { case (morse, line) => morse.map(_.ch).mkString ++ " " ++ line.map(_.ch).mkString }.asJava)
  }
}
