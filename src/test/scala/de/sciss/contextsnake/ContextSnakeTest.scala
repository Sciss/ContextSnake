package de.sciss.contextsnake

object ContextSnakeTest extends App {
  val test = ContextSnake( "BOOKKEEPER": _* )
  println(test.toDOT())
}