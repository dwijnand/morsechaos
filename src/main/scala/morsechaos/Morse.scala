package morsechaos

object Morse {
  val charToMorse: Map[Char, String] = Map(
    'a' -> ".-", 'b' -> "-...", 'c' -> "-.-.", 'd' -> "-..", 'e' -> ".", 'f' -> "..-.", 'g' -> "--.",
    'h' -> "....", 'i' -> "..", 'j' -> ".---", 'k' -> "-.-", 'l' -> ".-..", 'm' -> "--", 'n' -> "-.",
    'o' -> "---", 'p' -> ".--.", 'q' -> "--.-", 'r' -> ".-.", 's' -> "...", 't' -> "-", 'u' -> "..-",
    'v' -> "...-", 'w' -> ".--", 'x' -> "-..-", 'y' -> "-.--", 'z' -> "--.."
  )

  val morseToChar: Map[String, Char] = charToMorse.map(_.swap)


  def apply(ch: Char): String = encode(ch)
  def apply(s: String): String = encode(s)


  def encode(ch: Char): String = charToMorse(ch)
  def encode(s: String): String = s.flatMap(charToMorse(_))


  def decode(s: String): Char = morseToChar(s)


  def encodeOpt(ch: Char): Option[String] = charToMorse.get(ch)
  def encodeOpt(s: String): Option[String] =
    s.foldLeft(Option(""))((res, ch) => res.flatMap(res => charToMorse.get(ch).map(res + _)))

  def decodeOpt(s: String): Option[Char] = morseToChar.get(s)
}
