package de.sciss.contextsnake

object ExampleTrees extends App {
  def run(input: String, tailEdges: Boolean) {
    val c = ContextSnake(input: _*)
    println("For input '" + input + "':")
    print(c.toDOT(tailEdges = tailEdges))
    println()
  }

  run("BOOKKE",  tailEdges = false)
  run("ABABABC", tailEdges = true)
}