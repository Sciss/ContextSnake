/*
 *  DoubleLinkedList.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.contextsnake.txn.impl

import scala.annotation.{tailrec, switch}
import de.sciss.lucre.stm.{Mutable, Sys}
import de.sciss.serial.{DataOutput, DataInput, Serializer}
import de.sciss.lucre.data

object DoubleLinkedList {
  def empty[S <: Sys[S], A](implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, A]): DoubleLinkedList[S, A] =
    new Impl[S, A] {
      val id                = tx.newID()
      protected val sizeRef = tx.newIntVar(id, 0)
      protected val headRef = tx.newVar[C](id, null)(CellSer)
      protected val lastRef = tx.newVar[C](id, null)(CellSer)
    }

  def serializer[S <: Sys[S], A](implicit elemSerializer: Serializer[S#Tx, S#Acc, A]):
  Serializer[S#Tx, S#Acc, DoubleLinkedList[S, A]] =
    new Ser[S, A]

  def read[S <: Sys[S], A](in: DataInput, access: S#Acc)
                          (implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, A]): DoubleLinkedList[S, A] = {
    new Impl[S, A] {
      val id                = tx.readID(in, access)
      protected val sizeRef = tx.readIntVar(id, in)
      protected val headRef = tx.readVar[C](id, in)
      protected val lastRef = tx.readVar[C](id, in)
    }
  }

  private class Ser[S <: Sys[S], A](implicit elemSerializer: Serializer[S#Tx, S#Acc, A])
    extends Serializer[S#Tx, S#Acc, DoubleLinkedList[S, A]] {

    def write(ll: DoubleLinkedList[S, A], out: DataOutput): Unit = ll.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): DoubleLinkedList[S, A] =
      DoubleLinkedList.read(in, access)
  }

  private final class Cell[S <: Sys[S], A](val elem: A,
                                              val pred: S#Var[Cell[S, A]], val succ: S#Var[Cell[S, A]])

  private final class Iter[S <: Sys[S], A](private var cell: Cell[S, A]) extends data.Iterator[S#Tx, A] {
    override def toString = if (cell == null) "empty iterator" else "non-empty iterator"

    def hasNext(implicit tx: S#Tx) = cell != null

    def next()(implicit tx: S#Tx): A = {
      if (cell == null) throw new NoSuchElementException("next on empty iterator")
      val res = cell.elem
      cell    = cell.succ()
      res
    }
  }

  private abstract class Impl[S <: Sys[S], A](implicit protected val elemSerializer: Serializer[S#Tx, S#Acc, A])
    extends DoubleLinkedList[S, A] with Mutable.Impl[S] {

    list =>

    final protected type C = Cell[S, A]

    protected def headRef: S#Var[C]
    protected def lastRef: S#Var[C]
    protected def sizeRef: S#Var[Int]

    override def toString() = s"DoubleLinkedList$id"

    protected implicit object CellSer extends Serializer[S#Tx, S#Acc, C] {
      def write(cell: C, out: DataOutput): Unit =
        if (cell != null) {
          out.writeByte(1)
          elemSerializer.write(cell.elem, out)
          cell.pred.write(out)
          cell.succ.write(out)
        } else {
          out.writeByte(0)
        }

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): C = {
        (in.readByte: @switch) match {
          case 1 =>
            val elem = elemSerializer.read(in, access)
            val pred = tx.readVar[C](id, in)
            val succ = tx.readVar[C](id, in)
            new Cell[S, A](elem, pred, succ)
          case 0 => null
          case cookie => sys.error(s"Unexpected cookie $cookie")
        }
      }
    }

    final def indexOf(elem: A)(implicit tx: S#Tx): Int = {
      var idx = 0
      var rec = headRef()
      while (rec != null) {
        if (rec.elem == elem) return idx
        idx += 1
        rec = rec.succ()
      }
      -1
    }

    final def apply(idx: Int)(implicit tx: S#Tx): A =
      get(idx).getOrElse(throw new IndexOutOfBoundsException(idx.toString))

    final def get(idx: Int)(implicit tx: S#Tx): Option[A] = {
      if (idx < 0) return None
      var left = idx
      var rec = headRef()
      while (rec != null && left > 0) {
        left -= 1
        rec = rec.succ()
      }
      if (rec == null) None else Some(rec.elem)
    }

    final def append(elem: A)(implicit tx: S#Tx): Unit = {
      val pred      = lastRef()
      val succ      = null
      val idx       = sizeRef()
      insert(elem, pred, succ, idx)
    }

    final def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit = xs.foreach(append)

    final def prepend(elem: A)(implicit tx: S#Tx): Unit = {
      val pred      = null
      val succ      = headRef()
      val idx       = 0
      insert(elem, pred, succ, idx)
    }

    def insert(index: Int, elem: A)(implicit tx: S#Tx): Unit = {
      if (index < 0)      throw new IndexOutOfBoundsException(index.toString)
      var pred      = null: C
      var succ      = headRef()
      var idx       = 0
      while (idx < index) {
        if (succ == null) throw new IndexOutOfBoundsException(index.toString)
        pred  = succ
        succ  = succ.succ()
        idx  += 1
      }
      insert(elem, pred, succ, idx)
    }

    private def insert(elem: A, pred: C, succ: C, idx: Int)(implicit tx: S#Tx): Unit = {
      val recPred   = tx.newVar[C](id, pred)
      val recSucc   = tx.newVar[C](id, succ)
      val rec       = new Cell[S, A](elem, recPred, recSucc)
      val predSucc  = if (pred == null) headRef else pred.succ
      val succPred  = if (succ == null) lastRef else succ.pred
      predSucc()    = rec
      succPred()    = rec
      sizeRef.transform(_ + 1)
    }

    final protected def foreach(fun: A => Unit)(implicit tx: S#Tx): Unit = {
      @tailrec def loop(cell: C): Unit =
        if (cell != null) {
          fun(cell.elem)
          loop(cell.succ())
        }

      loop(headRef())
    }

    final def remove(elem: A)(implicit tx: S#Tx): Boolean = {
      var rec = headRef()
      var idx = 0
      while (rec != null) {
        if (rec.elem == elem) {
          removeCell(rec)
          return true
        }
        rec = rec.succ()
        idx += 1
      }
      false
    }

    final def removeAt(index: Int)(implicit tx: S#Tx): A = {
      if (index < 0) throw new IndexOutOfBoundsException(index.toString)
      var rec = headRef()
      if (rec == null) throw new IndexOutOfBoundsException(index.toString)
      var idx = 0
      while (idx < index) {
        rec = rec.succ()
        if (rec == null) throw new IndexOutOfBoundsException(index.toString)
        idx += 1
      }

      val e = rec.elem
      removeCell(rec)
      e
    }

    // unlinks a cell and disposes it. does not fire. decrements sizeRef
    private def removeCell(cell: C)(implicit tx: S#Tx): Unit = {
      val pred = cell.pred()
      val succ = cell.succ()
      if (pred != null) {
        pred.succ() = succ
      } else {
        headRef() = succ
      }
      if (succ != null) {
        succ.pred() = pred
      } else {
        lastRef() = pred
      }
      sizeRef.transform(_ - 1)
      disposeCell(cell)
    }

    final def removeLast()(implicit tx: S#Tx): A = {
      val rec = lastRef()
      if (rec == null) throw new NoSuchElementException("last of empty list")

      val pred  = rec.pred()
      val e     = rec.elem
      val idx   = sizeRef() - 1
      disposeCell(rec)
      sizeRef() = idx
      lastRef() = pred
      if (pred == null) {
        headRef() = null
      } else {
        pred.succ() = null
      }
      e
    }

    final def trimStart(n: Int)(implicit tx: S#Tx): Unit = {
      var i = n
      while (i > 0) {
        removeHead()
        i -= 1
      }
    }

    final def trimEnd(n: Int)(implicit tx: S#Tx): Unit = {
      var i = n
      while (i > 0) {
        removeLast()
        i -= 1
      }
    }

    final def removeHead()(implicit tx: S#Tx): A = {
      val rec = headRef()
      if (rec == null) throw new NoSuchElementException("head of empty list")

      val succ = rec.succ()
      val e = rec.elem
      disposeCell(rec)
      sizeRef.transform(_ - 1)
      headRef() = succ
      if (succ == null) {
        lastRef() = null
      } else {
        succ.pred() = null
      }
      e
    }

    final def clear()(implicit tx: S#Tx): Unit =
      while (nonEmpty) removeLast()

    private def disposeCell(cell: C)(implicit tx: S#Tx): Unit = {
      cell.pred.dispose()
      cell.succ.dispose()
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      var rec = headRef()
      while (rec != null) {
        val tmp = rec.succ()
        disposeCell(rec)
        rec = tmp
      }
      sizeRef.dispose()
      headRef.dispose()
      lastRef.dispose()
    }

    final protected def writeData(out: DataOutput): Unit = {
      sizeRef.write(out)
      headRef.write(out)
      lastRef.write(out)
    }

    final def isEmpty (implicit tx: S#Tx): Boolean = size == 0
    final def nonEmpty(implicit tx: S#Tx): Boolean = size > 0
    final def size    (implicit tx: S#Tx): Int     = sizeRef()

    final def headOption(implicit tx: S#Tx): Option[A] = {
      val rec = headRef()
      if (rec != null) Some(rec.elem) else None
    }

    final def lastOption(implicit tx: S#Tx): Option[A] = {
      val rec = lastRef()
      if (rec != null) Some(rec.elem) else None
    }

    //    final def head(implicit tx: S#Tx): A = {
    //      val rec = headRef()
    //      if (rec != null) rec.elem else throw new NoSuchElementException("head of empty list")
    //    }
    //
    //    final def last(implicit tx: S#Tx): A = {
    //      val rec = lastRef()
    //      if (rec != null) rec.elem else throw new NoSuchElementException("last of empty list")
    //    }

    final def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, A] = new Iter(headRef())
  }
}
trait DoubleLinkedList[S <: Sys[S], A] extends Mutable[S#ID, S#Tx] {
  def prepend(elem: A)(implicit tx: S#Tx): Unit
  def append (elem: A)(implicit tx: S#Tx): Unit
  def remove (elem: A)(implicit tx: S#Tx): Boolean
  def insert(idx: Int, elem: A)(implicit tx: S#Tx): Unit

  def apply(idx: Int)(implicit tx: S#Tx): A

  def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit

  def headOption(implicit tx: S#Tx): Option[A]
  def lastOption(implicit tx: S#Tx): Option[A]

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  def size(implicit tx: S#Tx): Int

  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, A]

  def trimStart(n: Int)(implicit tx: S#Tx): Unit
  def trimEnd  (n: Int)(implicit tx: S#Tx): Unit

  def removeHead()(implicit tx: S#Tx): A
  def removeLast()(implicit tx: S#Tx): A
}