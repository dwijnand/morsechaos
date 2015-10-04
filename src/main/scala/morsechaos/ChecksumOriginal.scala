package morsechaos

import scala.collection.JavaConverters._
import java.nio.file._
import java.security.MessageDigest

object ChecksumOriginal {
  def main(args: Array[String]): Unit = {
    val bytes = Files.readAllBytes(Paths.get("signs.txt"))

    val incomingMd5 = Files.readAllLines(Paths.get("signs.txt.md5")).asScala.head.split(' ').head
    val expectedMd5 = calcMd5(bytes)
    require(incomingMd5 == expectedMd5, s"MD5 mismatch: $incomingMd5 != $expectedMd5")

    val incomingSha1 = Files.readAllLines(Paths.get("signs.txt.sha1")).asScala.head.split(' ').head
    val expectedSha1 = calcSha1(bytes)
    require(incomingSha1 == expectedSha1, s"SHA1 mismatch: $incomingSha1 != $expectedSha1")

    println("OK, checksums match")
  }

  def calcMd5(bytes: Array[Byte]): String    = calcCksum(md5er, bytes)
  def calcSha1(bytes: Array[Byte]): String   = calcCksum(sha1er, bytes)
  def calcSha256(bytes: Array[Byte]): String = calcCksum(sha256er, bytes)

  private def calcCksum(md: MessageDigest, bytes: Array[Byte]): String = { md update bytes ; hex(md.digest) }

  def md5er    = MessageDigest getInstance "MD5"
  def sha1er   = MessageDigest getInstance "SHA-1"
  def sha256er = MessageDigest getInstance "SHA-256"

  /** Returns the hexadecimal string form of the specified byte array. */
  def hex(bytes: Array[Byte]) = bytes.map(b => "%02x" format b) mkString ""
}
