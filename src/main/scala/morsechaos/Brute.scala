package morsechaos

object Brute {
  def main(args: Array[String]): Unit = {
    val l = """
em is that this is not the case
the other way to solve this
is to make a computer program that tries different combinations of letters using a backtracking
algorithm
the program should be clever enough to avoid some wrong patterns
for example if there are three dashes your program could think that there are three times the letter t
but this is wrong because in normal english is very strange to find three letters t together
these kind of things reduces the number of combinations but there are still too many possibilities
    """.trim.stripMargin filter (_ != ' ') filter (_ != '\n')
    val c = "thus it is up to you work with di"
    val r = """
grams trigrams or tetra grams frequencies and you can solve this automatically
    """.trim.stripMargin filter (_ != ' ') filter (_ != '\n')
    val expected = "cc848bf37c77a3c6283bbc6c1ffc086b"
    import com.timgroup.iterata.ParIterator.Implicits._
    println(s"$isoTime: Start getting poss")
    val poss = morsechaos.MorseSeg resplit c
    println(s"$isoTime: Done  getting poss")
    poss.par() foreach {s => println(s); if (Md5((l + s + r).take(512)) == expected) sys error s"FOUND: $s" }
  }

  def isoTime() = {
    val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    df setTimeZone (java.util.TimeZone getTimeZone "UTC")
    df format new java.util.Date()
  }
}
