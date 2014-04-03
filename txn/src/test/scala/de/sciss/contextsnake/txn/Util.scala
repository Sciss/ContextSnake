package de.sciss.contextsnake.txn

import annotation.{elidable, tailrec}
import de.sciss.lucre.stm.Sys
import scala.collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.geom.Space

object Util {
  def expandWhileChoice[S <: Sys[S], D <: Space[D], A](s: ContextTree.Snake[S, D, A], minChoice: Int = 2,
                                                       maxLength: Int = 1000)
                          (implicit tx: S#Tx, random: TxnRandom[S#Tx], ord: Ordering[A]): Vec[A] = {
    @tailrec def loop(i: Int): Unit = {
      if (i == maxLength) return
      val sq  = s.successors.toIndexedSeq.sorted
      val sz  = sq.size
      if (sz < minChoice) return
      val idx = (random.nextDouble() * sz).toInt
      s += sq(idx)
      loop(i + 1)
    }
    loop(0)
    s.iterator.toIndexedSeq
  }

  def produce[S <: Sys[S], D <: Space[D], A](t: ContextTree[S, D, A], len: Int = 100, maxSingleChoice: Int = 2)
                (init: TraversableOnce[A])(implicit tx: S#Tx, random: TxnRandom[S#Tx], ord: Ordering[A]): Vec[A] = {
    val s = t.snake(init)
    val b = Vector.newBuilder[A]
    b.sizeHint(len)
    var off = 0
    var singleChoice = 0
    init.foreach { e => b += e; off += 1 }
    while (off < len && s.nonEmpty) {
      val sq = s.successors.toIndexedSeq.sorted

      //      locally {
      //        import de.sciss.lucre.geom.{QueryShape, IntRectangle}
      //        if (s.nonEmpty) {
      //          val e     = s.apply(s.length - 1)
      //          val c     = e.asInstanceOf[Char].toInt
      //          val shape = IntRectangle(c - 4, 0, 8, 256)
      //          // s.trimEnd(1)
      //          val test = s.successorsRange(shape.asInstanceOf[QueryShape[_, D]]).toIndexedSeq.sorted
      //          // s += e  // restore
      //          println(s"-- point successors: ${sq  .mkString(", ")}")
      //          println(s"-- range successors: ${test.mkString(", ")}")
      //        }
      //      }

      val sz = sq.size
      if (sz == 0 || sz == 1 && singleChoice == maxSingleChoice) {
        s.trimStart(1)
      } else {
        val elem = if (sz == 1) {
          singleChoice += 1
          sq.head
        } else {
          singleChoice = 0
          val idx = (random.nextDouble() * sz).toInt
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