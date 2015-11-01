package morsechaos

object Md5DrivenSolver {
  val expectedMd5_2 = "cc848bf37c77a3c6283bbc6c1ffc086b"
  val expectedMd5_3 = "c5e27d798bdc03c71e9c5362ba239783"

  def md5StrToBytes(md5: String) = md5 grouped 2 map (s => ((s(0).asDigit << 4) + s(1).asDigit).toByte) toArray

  val expectedMd5Bytes_2 = md5StrToBytes(expectedMd5_2)
  val expectedMd5Bytes_3 = md5StrToBytes(expectedMd5_3)

  def main(args: Array[String]): Unit = {
    ()
  }
}

final class Md5DrivenIterator(val idx0: BigInt, val bytes0: Array[Byte]) extends Iterator[(BigInt, Array[Byte])] {
  private var _hasNext = true

  def hasNext: Boolean = _hasNext

  val a = 'a'.toByte
  val z = 'z'.toByte

  val len = bytes0.length
  val indices = bytes0.indices

  private var idx = idx0
  private var bytes = bytes0

  def next(): (BigInt, Array[Byte]) = {
    ???
  }
}

object Md5_2 {
  def apply(s: String): String = apply(s getBytes "UTF-8")
  def apply(bytes: Seq[Byte]): String = {
    val md = java.security.MessageDigest getInstance "MD5"
    md update bytes.toArray
    md.digest.iterator map (b => f"$b%02x") mkString ""
  }
}
