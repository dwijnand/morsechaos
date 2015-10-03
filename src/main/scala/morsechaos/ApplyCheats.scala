package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object ApplyCheats {
  def main(args: Array[String]): Unit = {
    val text = Files.readAllLines(Paths.get("signs.clean.txt")).asScala.mkString

    val str1 = "ENGLISH IS VERY"
    val str1morse = str1.filter(_ != ' ').map(Letter.unsafe).flatMap(Morse.encodeLetter).map(_.ch).mkString
    val len = str1morse.length
    val index = text.indexOfSlice(str1morse)
    require(text.indexOfSlice(str1morse, index + 1) == -1)
    val newText = text.slice(0, index) ++ s" $str1 " ++ text.slice(index + len, text.length)

    val str2 = "DID NOT TRIED"
    val str2morse = str2.filter(_ != ' ').map(Letter.unsafe).flatMap(Morse.encodeLetter).map(_.ch).mkString
    val len2 = str2morse.length
    val index2 = newText.indexOfSlice(str2morse)
    require(newText.indexOfSlice(str2morse, index2 + 1) == -1)
    val newText2 = newText.slice(0, index2) ++ s" $str2 " ++ newText.slice(index2 + len2, newText.length)

    Files.write(Paths.get("signs.1.txt"), newText2.grouped(118).toSeq.asJava)
    ()
  }
}
