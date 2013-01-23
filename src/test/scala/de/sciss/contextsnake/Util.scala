package de.sciss.contextsnake

import annotation.tailrec

object Util {
  def expandWhileChoice[A](s: ContextTree.Snake[A]): Vector[A] = {
    @tailrec def loop() {
      val sq  = s.successors.to[Vector]
      val sz  = sq.size
      if (sz <= 1) return
      val idx = (math.random * sz).toInt
      s += sq(idx)
      loop()
    }
    loop()
    s.to[Vector]
  }
}