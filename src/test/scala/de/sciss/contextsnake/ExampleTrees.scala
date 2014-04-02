package de.sciss.contextsnake

// note: doesn't work with elision
object ExampleTrees extends App {
  def run(input: String, tailEdges: Boolean): Unit = {
    val c = ContextTree(input: _*)
    println("For input '" + input + "':")
    print(c.toDOT(tailEdges = tailEdges))
    println()
  }

  run("BOOKKE",  tailEdges = false)
  run("ABABABC", tailEdges = true)
}