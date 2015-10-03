package morsechaos

sealed abstract class Letter(val ch: Char) {
  override val toString = ch.toString
}

case object A extends Letter('A'); case object B extends Letter('B'); case object C extends Letter('C')
case object D extends Letter('D'); case object E extends Letter('E'); case object F extends Letter('F')
case object G extends Letter('G'); case object H extends Letter('H'); case object I extends Letter('I')
case object J extends Letter('J'); case object K extends Letter('K'); case object L extends Letter('L')
case object M extends Letter('M'); case object N extends Letter('N'); case object O extends Letter('O')
case object P extends Letter('P'); case object Q extends Letter('Q'); case object R extends Letter('R')
case object S extends Letter('S'); case object T extends Letter('T'); case object U extends Letter('U')
case object V extends Letter('V'); case object W extends Letter('W'); case object X extends Letter('X')
case object Y extends Letter('Y'); case object Z extends Letter('Z')

object Letter {
  val letterCharToLetter = Map(
    'A' -> A, 'B' -> B, 'C' -> C, 'D' -> D, 'E' -> E, 'F' -> F, 'G' -> G, 'H' -> H, 'I' -> I, 'J' -> J,
    'K' -> K, 'L' -> L, 'M' -> M, 'N' -> N, 'O' -> O, 'P' -> P, 'Q' -> Q, 'R' -> R, 'S' -> S, 'T' -> T,
    'U' -> U, 'V' -> V, 'W' -> W, 'X' -> X, 'Y' -> Y, 'Z' -> Z
  )
  def fromChar(ch: Char): Option[Letter] = letterCharToLetter.get(ch)
  def unsafe(ch: Char): Letter = letterCharToLetter(ch)
}

sealed class Tone(val ch: Char) { override final val toString = ch.toString }
case object Dit extends Tone('.')
case object Dah extends Tone('-')

object Tone {
  val toneCharToTone = Map('.' -> Dit, '-' -> Dah)
  def fromChar(ch: Char): Option[Tone] = toneCharToTone.get(ch)
  def unsafe(ch: Char): Tone = toneCharToTone(ch)
}

object Morse {
  val charToMorse: Map[Letter, Vector[Tone]] =
    Vector(
      A -> ".-", B -> "-...", C -> "-.-.", D -> "-..", E -> ".", F -> "..-.", G -> "--.", H -> "....",
      I -> "..", J -> ".---", K -> "-.-", L -> ".-..", M -> "--", N -> "-.", O -> "---", P -> ".--.",
      Q -> "--.-", R -> ".-.", S -> "...", T -> "-", U -> "..-", V -> "...-", W -> ".--", X -> "-..-",
      Y -> "-.--", Z -> "--.."
    ).map { case (l, morse) => l -> morse.toVector.map(Tone.unsafe) }.toMap

  val morseToChar: Map[Vector[Tone], Letter] = charToMorse.map(_.swap)

  def encodeLetter(l: Letter): Vector[Tone] = charToMorse(l)
  def decodeTones(s: Vector[Tone]): Option[Letter] = morseToChar get s
}
