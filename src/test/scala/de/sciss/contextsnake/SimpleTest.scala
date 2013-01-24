package de.sciss.contextsnake

object SimpleTest extends App {
  val c = ContextTree("BAAB": _*)
  val res = c.containsSlice("AB")
  println(res)
}