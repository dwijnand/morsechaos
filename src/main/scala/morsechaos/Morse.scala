package morsechaos

object Morse {
  val charToMorse = Map(
    ('a', ".-"), ('b', "-..."), ('c', "-.-."), ('d', "-.."), ('e', "."), ('f', "..-."), ('g', "--."),
    ('h', "...."), ('i', ".."), ('j', ".---"), ('k', "-.-"), ('l', ".-.."), ('m', "--"), ('n', "-."),
    ('o', "---"), ('p', ".--."), ('q', "--.-"), ('r', ".-."), ('s', "..."), ('t', "-"), ('u', "..-"),
    ('v', "...-"), ('w', ".--"), ('x', "-..-"), ('y', "-.--"), ('z', "--..")
  )

  val morseToChar = charToMorse.map(_.swap)

  def encode(ch: Char): Option[String] = charToMorse get ch
  def decode(s: String): Option[Char]  = morseToChar get s
}
