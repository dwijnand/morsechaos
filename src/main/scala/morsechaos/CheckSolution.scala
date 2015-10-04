package morsechaos

import scala.collection.JavaConverters._
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.security.MessageDigest

object CheckSolution {
  def main(args: Array[String]): Unit = {
    val text = Files.readAllLines(Paths.get("signs.clean.txt")).asScala.mkString
    val noSpaces = text.filter(_ != ' ')
    val allBytes = noSpaces.getBytes(StandardCharsets.UTF_8)
    val bytesGroups = allBytes.grouped(512).toSeq

    val bytes1 = bytesGroups.applyOrElse(0, (_: Int) => Array.empty[Byte])
    val bytes2 = bytesGroups.applyOrElse(1, (_: Int) => Array.empty[Byte])
    val bytes3 = bytesGroups.applyOrElse(2, (_: Int) => Array.empty[Byte])
    val bytes4 = bytesGroups.applyOrElse(3, (_: Int) => Array.empty[Byte])

    val incomingBytes1Md5 = calcMd5(bytes1)
    val incomingBytes2Md5 = calcMd5(bytes2)
    val incomingBytes3Md5 = calcMd5(bytes3)
    val incomingBytes4Md5 = calcMd5(bytes4)

    def assertMd5(incoming: String, expected: String, n: Int) =
      require(incoming == expected, s"MD5 mismatch for bytes$n: $incoming != $expected")

    assertMd5(incomingBytes1Md5, "2d9ce6581ebe66d83053b696ef91aea2", 1)
    assertMd5(incomingBytes2Md5, "cc848bf37c77a3c6283bbc6c1ffc086b", 2)
    assertMd5(incomingBytes3Md5, "c5e27d798bdc03c71e9c5362ba239783", 3)
    assertMd5(incomingBytes4Md5, "693ab2655d16e06d29fef3240c745bd0", 4)
    ()
  }

  def calcMd5(bytes: Array[Byte]): String = calcCksum(md5er, bytes)

  private def calcCksum(md: MessageDigest, bytes: Array[Byte]): String = { md update bytes ; hex(md.digest) }

  def md5er    = MessageDigest getInstance "MD5"

  /** Returns the hexadecimal string form of the specified byte array. */
  def hex(bytes: Array[Byte]) = bytes.map(b => "%02x" format b) mkString ""
}
