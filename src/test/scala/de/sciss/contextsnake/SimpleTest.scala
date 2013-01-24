package de.sciss.contextsnake

object SimpleTest extends App {
  val c = ContextTree("BOOKKEEPER": _*)
  println(c.containsSlice("EE"))
}