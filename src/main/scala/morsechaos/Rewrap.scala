package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object Rewrap {
  def main(args: Array[String]): Unit = {
    val text = Files.readAllLines(Paths get args.head).asScala.mkString
    Files.write(Paths.get(args.head), text.grouped(118).toSeq.asJava)
  }
}
