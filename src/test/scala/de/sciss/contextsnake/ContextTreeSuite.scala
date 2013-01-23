package de.sciss.contextsnake

import org.scalatest.FunSuite

class ContextTreeSuite extends FunSuite {
  val seed  = 0L
  val N     = 8 // 10000 // corpus size. higher than 10k gets slow because of the `.tails` iterations
  val M     = 2  // 26    // maximum number of different symbols

  test("sub-sequences searches are performed on the suffix tree") {
    val r   = new util.Random(seed)
    val v   = Vector.fill(N)(r.nextInt(M))
    val c   = ContextTree(v: _*)
    val t1  = v.tails.forall(c.containsSlice)
    assert(t1)

    assert(c.size === v.size)
    val t2  = v.tails.filter(_.nonEmpty).map { xs =>
      val idx = r.nextInt(xs.size)
      xs.updated(idx, M)
    }
    val t3  = t2.exists(c.containsSlice)
    assert(!t3)
  }
}