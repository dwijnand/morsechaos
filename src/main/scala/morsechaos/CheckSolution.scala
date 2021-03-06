package morsechaos

import scala.collection.JavaConverters._
import java.nio.charset.StandardCharsets
import java.nio.file._

object CheckSolution {
  val expectedMd5_1 = "2d9ce6581ebe66d83053b696ef91aea2"
  val expectedMd5_2 = "cc848bf37c77a3c6283bbc6c1ffc086b"
  val expectedMd5_3 = "c5e27d798bdc03c71e9c5362ba239783"
  val expectedMd5_4 = "693ab2655d16e06d29fef3240c745bd0"

  def main(args: Array[String]): Unit = {
    val text = Files.readAllLines(Paths.get("wip.txt")).asScala.mkString
    val noSpaces = text.filter(_ != ' ')

 // val groups = noSpaces.grouped(512).toSeq
 // val chunk1 = groups.applyOrElse(0, (_: Int) => "")
 // val chunk2 = groups.applyOrElse(1, (_: Int) => "")
 // val chunk3 = groups.applyOrElse(2, (_: Int) => "")
 // val chunk4 = groups.applyOrElse(3, (_: Int) => "")

    val (chunk1, rem1) = noSpaces splitAt 512
    val (rem2, chunk4) = rem1 splitAt rem1.length - 221
    val chunk2 = rem2 take 512
    val chunk3 = rem2 takeRight 512

    testChunk(chunk1, expectedMd5_1, 1)
    testChunk(chunk2, expectedMd5_2, 2)
    testChunk(chunk3, expectedMd5_3, 3)
    testChunk(chunk4, expectedMd5_4, 4)
    ()
  }

  def testChunk(text: String, expectedMd5: String, n: Int) = {
    println(s"text $n: $text")
    val bytes = text.getBytes(StandardCharsets.UTF_8)
    assert(bytes.length == text.length)
    assertMd5(Md5(bytes), expectedMd5, n)
    println()
  }

  val grnCheckMark = scala.io.AnsiColor.GREEN + "\u2713" + scala.io.AnsiColor.RESET
  val redCrossMark = scala.io.AnsiColor.RED   + "\u2717" + scala.io.AnsiColor.RESET

  def assertMd5(incoming: String, expected: String, n: Int) =
    if (incoming == expected)
      println(s"MD5 MATCH!!! $grnCheckMark for bytes$n: incoming: $incoming == expected: $expected")
    else
      println(s"MD5 mismatch $redCrossMark for bytes$n: incoming: $incoming != expected: $expected")
}
