package de.sciss.contextsnake

import annotation.{elidable, tailrec}

object Util {
  def expandWhileChoice[A](s: ContextTree.Snake[A], minChoice: Int = 2, maxLength: Int = 1000): Vector[A] = {
    @tailrec def loop(i: Int) {
      if (i == maxLength) return
      val sq  = s.successors.to[Vector]
      val sz  = sq.size
      if (sz < minChoice) return
      val idx = (math.random * sz).toInt
      s += sq(idx)
      loop(i + 1)
    }
    loop(0)
    s.to[Vector]
  }

  @elidable(elidable.INFO) private def printElision() {
    println ("INFO level logging enabled.")
  }

  def elision() {
    printElision()
  }
}