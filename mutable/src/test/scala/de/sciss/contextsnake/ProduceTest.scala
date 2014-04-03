package de.sciss.contextsnake

/*
  to run

  contextsnake-mutable/test:run

 */
object ProduceTest extends App {
  // currently has a problem at index 190 or 146 (upcase)
  val txt =
    """Wer reitet so spaet durch Nacht und Wind?
      |Es ist der Vater mit seinem Kind.
      |Er hat den Knaben wohl in dem Arm,
      |Er fasst ihn sicher, er haelt ihn warm.
      |
      |Mein Sohn, was birgst du so bang dein Gesicht?
      |Siehst Vater, du den Erlkoenig nicht!
      |Den Erlenkoenig mit Kron' und Schweif?
      |Mein Sohn, es ist ein Nebelstreif.
      |
      |Du liebes Kind, komm geh' mit mir!
      |Gar schoene Spiele, spiel ich mit dir,
      |Manch bunte Blumen sind an dem Strand,
      |Meine Mutter hat manch guelden Gewand.
      |
      |Mein Vater, mein Vater, und hoerest du nicht,
      |Was Erlenkoenig mir leise verspricht?
      |Sei ruhig, bleibe ruhig, mein Kind,
      |In duerren Blaettern saeuselt der Wind.
      |
      |Willst feiner Knabe du mit mir geh'n?
      |Meine Toechter sollen dich warten schoen,
      |Meine Toechter fuehren den naechtlichen Reihn
      |Und wiegen und tanzen und singen dich ein.
      |
      |Mein Vater, mein Vater, und siehst du nicht dort
      |Erlkoenigs Toechter am duesteren Ort?
      |Mein Sohn, mein Sohn, ich seh'es genau:
      |Es scheinen die alten Weiden so grau.
      |
      |Ich lieb dich, mich reizt deine schoene Gestalt,
      |Und bist du nicht willig, so brauch ich Gewalt!
      |Mein Vater, mein Vater, jetzt fasst er mich an,
      |Erlkoenig hat mir ein Leids getan.
      |
      |Dem Vater grauset's, er reitet geschwind,
      |Er haelt in den Armen das aechzende Kind,
      |Erreicht den Hof mit Muehe und Not,
      |In seinen Armen das Kind war tot.""".stripMargin

  locally {
    val r   = new util.Random(5678L)
    val res = Vector.fill(20)((r.nextDouble() * 100).toInt)
    println("util.Random:")
    println(res)
    // Vector(95, 32, 62, 76, 40, 97, 68, 23, 62, 43, 52, 64, 85, 21, 23, 33, 60, 5, 48, 60)
    println()
  }

  val c = ContextTree(txt.toUpperCase: _*)
  implicit val r = new util.Random(5678L)
  val res = Util.produce(c,200,4)("M").mkString
  println(res)

  /*
    MUEHE UMEN SO GRAND,
    MEINE TOECHTLICHEN REITET SO SPIEL ICH GEWALEIBE RUHIG, MEIN VATER, DU DEN HOF MIT DIR,
    IN DUERRGST DU NICHT WILLIEBES KINGEN DIE ALTEN WEIDEN SIND AN,
    ERLKOENICHT,
    WASST IHN SCHO
   */
}
