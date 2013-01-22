package de.sciss.contextsnake

object ContextSnakeTest extends App {
  val test = ContextSnake( "BOOKKE": _* )
  println(test.toDOT())
}