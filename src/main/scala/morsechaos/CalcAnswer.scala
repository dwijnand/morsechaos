package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object CalcAnswer {
  def main(args: Array[String]): Unit = {
    val text = Files.readAllLines(Paths get "wip.txt").asScala.mkString filter (_ != ' ')
    val len = text.length
    val sum = text.map(ch => ch - 'a' + 1).sum
    val answer = len * sum
    println(s"$len * $sum: $answer")
    ()
  }
}
