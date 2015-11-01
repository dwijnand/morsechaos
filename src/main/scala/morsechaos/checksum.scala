package morsechaos

object Md5 {
  def apply(s: String): String = apply(s getBytes "UTF-8")
  def apply(bytes: Seq[Byte]): String = {
    val md = java.security.MessageDigest getInstance "MD5"
    md update bytes.toArray
    md.digest.iterator map (b => f"$b%02x") mkString ""
  }
}

object Sha1 {
  def apply(s: String): String = apply(s getBytes "UTF-8")
  def apply(bytes: Seq[Byte]): String = {
    val md = java.security.MessageDigest getInstance "SHA-1"
    md update bytes.toArray
    md.digest.iterator map (b => f"$b%02x") mkString ""
  }
}

object Sha256 {
  def apply(s: String): String = apply(s getBytes "UTF-8")
  def apply(bytes: Seq[Byte]): String = {
    val md = java.security.MessageDigest getInstance "SHA-256"
    md update bytes.toArray
    md.digest.iterator map (b => f"$b%02x") mkString ""
  }
}
