package de.sciss.contextsnake

import annotation.{elidable, tailrec}

object Util {
  def expandWhileChoice[A](s: ContextTree.Snake[A], minChoice: Int = 2, maxLength: Int = 1000): Vector[A] = {
    @tailrec def loop(i: Int): Unit = {
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

  def produce[A](t: ContextTree[A], len: Int = 100, maxSingleChoice: Int = 2)(init: TraversableOnce[A]): Vector[A] = {
    val s = t.snake(init)
    val b = Vector.newBuilder[A]
    b.sizeHint(len)
    var off = 0
    var singleChoice = 0
    init.foreach { e => b += e; off += 1 }
    while (off < len && s.nonEmpty) {
      val sq = s.successors.to[Vector]
      val sz = sq.size
      if (sz == 0 || sz == 1 && singleChoice == maxSingleChoice) {
        s.trimStart(1)
      } else {
        val elem = if (sz == 1) {
          singleChoice += 1
          sq.head
        } else {
          singleChoice = 0
          val idx = (math.random * sz).toInt
          sq(idx)
        }
        s += elem
        b += elem
        off += 1
      }
    }
    b.result()
  }

  @elidable(elidable.INFO) private def printElision(): Unit =
    println("INFO level logging enabled.")

  def elision(): Unit = printElision()
}