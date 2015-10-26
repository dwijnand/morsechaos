package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._

object ChecksumOriginal {
  def main(args: Array[String]): Unit = {
    val bytes = Files.readAllBytes(Paths.get("signs.txt"))

    val incomingMd5 = Files.readAllLines(Paths.get("signs.txt.md5")).asScala.head.split(' ').head
    val expectedMd5 = Md5(bytes)
    require(incomingMd5 == expectedMd5, s"MD5 mismatch: $incomingMd5 != $expectedMd5")

    val incomingSha1 = Files.readAllLines(Paths.get("signs.txt.sha1")).asScala.head.split(' ').head
    val expectedSha1 = Sha1(bytes)
    require(incomingSha1 == expectedSha1, s"SHA1 mismatch: $incomingSha1 != $expectedSha1")

    println("OK, checksums match")
  }
}
