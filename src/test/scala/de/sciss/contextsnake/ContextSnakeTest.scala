package de.sciss.contextsnake

object ContextSnakeTest extends App {
  val test = ContextSnake( "ABABABC": _* )
  println(test.toDOT(tailEdges = true))
}