package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object ApplySpellCheck {
  def main(args: Array[String]): Unit = {
    val text = Files.readAllLines(Paths.get("signs.clean.txt")).asScala.mkString

    val str1 = "english is very"
    val str1morse = str1.filter(_ != ' ').map(Morse.encode).mkString
    val len = str1morse.length
    val index = text.indexOfSlice(str1morse)
    require(text.indexOfSlice(str1morse, index + 1) == -1)
    val newText = text.slice(0, index) + Morse.encode("englishitisvery") + text.slice(index + len, text.length)

    val str2 = "did not tried"
    val str2morse = str2.filter(_ != ' ').map(Morse.encode).mkString
    val len2 = str2morse.length
    val index2 = newText.indexOfSlice(str2morse)
    require(newText.indexOfSlice(str2morse, index2 + 1) == -1)
    val newText2 = newText.slice(0, index2) + Morse.encode("didnottry") + newText.slice(index2 + len2, newText.length)

    Files.write(Paths.get("signs.spckd.txt"), newText2.grouped(118).toSeq.asJava)
    ()
  }
}
