/*
 *  LinkedList.scala
 *  (LucreSTM)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.contextsnake.txn.impl

import annotation.tailrec
import de.sciss.lucre.stm.{Mutable, Sys}
import de.sciss.serial.{DataOutput, DataInput, Serializer}
import de.sciss.lucre.data

object LinkedList {
  def empty[S <: Sys[S], A](implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, A]): LinkedList[S, A] = {
    val id = tx.newID()
    val headRef = tx.newVar[Option[Cell[S, A]]](id, None)
    new Impl[S, A](id, headRef)
  }

  def apply[S <: Sys[S], A](elems: A*)(implicit tx: S#Tx,
                                       elemSerializer: Serializer[S#Tx, S#Acc, A]): LinkedList[S, A] = {
    val res = empty[S, A]
    res.appendAll(elems)
    res
  }

  implicit def serializer[S <: Sys[S], A](implicit elemSerializer: Serializer[S#Tx, S#Acc, A]):
    Serializer[S#Tx, S#Acc, LinkedList[S, A]] = new Ser[S, A]

  private final class Ser[S <: Sys[S], A](implicit elemSerializer: Serializer[S#Tx, S#Acc, A])
    extends Serializer[S#Tx, S#Acc, LinkedList[S, A]] {

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LinkedList[S, A] = {
      val id      = tx.readID(in, access)
      val headRef = tx.readVar[Option[Cell[S, A]]](id, in)
      new Impl[S, A](id, headRef)
    }

    def write(ll: LinkedList[S, A], out: DataOutput): Unit = ll.write(out)
  }

  def read[S <: Sys[S], A](in: DataInput, access: S#Acc)
                          (implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, A]): LinkedList[S, A] = {
    val id = tx.readID(in, access)
    val headRef = tx.readVar[Option[Cell[S, A]]](id, in)
    new Impl[S, A](id, headRef)
  }

  private final class Impl[S <: Sys[S], A](val id: S#ID, headRef: S#Var[Option[Cell[S, A]]])
                                          (implicit elemSerializer: Serializer[S#Tx, S#Acc, A])
    extends LinkedList[S, A] with Mutable.Impl[S] {

    override def toString() = s"LinkedList$id"

    protected def disposeData()(implicit tx: S#Tx): Unit =
      headRef.dispose()

    protected def writeData(out: DataOutput): Unit =
      headRef.write(out)

    def prepend(elem: A)(implicit tx: S#Tx): Unit = {
      val cellID  = tx.newID()
      val nextRef = tx.newVar(cellID, headRef())
      val c = new Cell[S, A](cellID, nextRef, elem)
      headRef() = Some(c)
    }

    def append(elem: A)(implicit tx: S#Tx): Unit = appendAll(elem :: Nil)

    def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit = {
      @tailrec def step(v: S#Var[Option[Cell[S, A]]]): Unit =
        v() match {
          case None =>
            var w = v
            xs.foreach { elem =>
              val cellID  = tx.newID()
              val nextRef = tx.newVar(cellID, Option.empty[Cell[S, A]])
              val c       = new Cell[S, A](cellID, nextRef, elem)
              w()         = Some(c)
              w           = nextRef
            }
          case Some(cell) =>
            step(cell.nextRef)
        }

      step(headRef)
    }

    def remove(elem: A)(implicit tx: S#Tx): Boolean = {
      @tailrec def step(v: S#Var[Option[Cell[S, A]]]): Boolean =
        v() match {
          case None => false
          case Some(cell) =>
            if (cell.value == elem) {
              v() = cell.nextRef()
              cell.dispose()
              true

            } else step(cell.nextRef)
        }

      step(headRef)
    }

    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, A] = new data.Iterator[S#Tx, A] {
      private var nextOption: Option[Cell[S, A]] = headRef()

      def next()(implicit tx: S#Tx): A = {
        val res = nextOption.getOrElse(throw new NoSuchElementException("next on empty iterator"))
        nextOption = res.nextRef()
        res.value
      }

      def hasNext(implicit tx: S#Tx): Boolean = nextOption.isDefined
    }

    def apply(idx: Int)(implicit tx: S#Tx): A = {
      if (idx < 0) throw new IndexOutOfBoundsException(idx.toString)

      @tailrec def step(i: Int, v: S#Var[Option[Cell[S, A]]]): A =
        v() match {
          case None => throw new IndexOutOfBoundsException(i.toString)
          case Some(cell) =>
            if (i == 0) cell.value
            else step(i - 1, cell.nextRef)
        }

      step(idx, headRef)
    }

    def headOption(implicit tx: S#Tx): Option[A] = headRef().map(_.value)

    def trimStart(n: Int)(implicit tx: S#Tx): Unit = {
      if (n < 0) throw new IndexOutOfBoundsException(n.toString)
      if (n == 0) return

      @tailrec def step(i: Int, v: S#Var[Option[Cell[S, A]]]): Option[Cell[S, A]] = {
        val opt = v()
        if (i == 0) opt
        else {
          val next = opt.getOrElse(throw new IndexOutOfBoundsException(n.toString))
          step(i - 1, next.nextRef)
        }
      }

      val sink  = step(n, headRef)
      headRef() = sink
    }

    def trimEnd  (n: Int)(implicit tx: S#Tx): Unit = {
      if (n < 0) throw new IndexOutOfBoundsException(n.toString)
      if (n == 0) return

      val buf = new Array[Option[Cell[S, A]]](n)

      @tailrec def step(i: Int, v: S#Var[Option[Cell[S, A]]]): Int = {
        val opt = v()
        buf(i % n) = opt
        opt match {
          case None => i
          case Some(c) =>
            step(i + 1, c.nextRef)
        }
      }

      val sz  = step(0, headRef)
      val off = sz - n
      if (off < 0) throw new IndexOutOfBoundsException(n.toString)
      headRef() = buf(off % n)
    }

    def size(implicit tx: S#Tx): Int = {
      @tailrec def step(i: Int, v: S#Var[Option[Cell[S, A]]]): Int =
        v() match {
          case None => i
          case Some(cell) => step(i + 1, cell.nextRef)
        }

      step(0, headRef)
    }

    def isEmpty(implicit tx: S#Tx): Boolean = headRef().isEmpty

    def nonEmpty(implicit tx: S#Tx): Boolean = !isEmpty
  }

  private implicit def cellSer[S <: Sys[S], A](implicit elemSerializer:
    Serializer[S#Tx, S#Acc, A]): Serializer[S#Tx, S#Acc, Cell[S, A]] = new CellSer[S, A]

  private final class CellSer[S <: Sys[S], A](implicit elemSerializer: Serializer[S#Tx, S#Acc, A])
    extends Serializer[S#Tx, S#Acc, Cell[S, A]] {

    implicit def self = this

    def write(v: Cell[S, A], out: DataOutput) {
      v.write(out)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Cell[S, A] = {
      val id = tx.readID(in, access)
      val nextRef = tx.readVar[Option[Cell[S, A]]](id, in)
      val value = elemSerializer.read(in, access)
      new Cell[S, A](id, nextRef, value)
    }
  }

  private final class Cell[S <: Sys[S], A](val id: S#ID, val nextRef: S#Var[Option[Cell[S, A]]], val value: A)
                                          (implicit elemSerializer: Serializer[S#Tx, S#Acc, A])
    extends Mutable.Impl[S] {
    protected def disposeData()(implicit tx: S#Tx) {
      nextRef.dispose()
    }

    protected def writeData(out: DataOutput) {
      nextRef.write(out)
      elemSerializer.write(value, out)
    }
  }

}

/** A transactional and mutable single linked list.
  *
  * The following operations are O(1): `prepend`, `isEmpty`, `nonEmpty`
  * The following operations are O(n): `append`, `remove`
  *
  * @tparam A   the element type stored in the list
  */
trait LinkedList[S <: Sys[S], A] extends Mutable[S#ID, S#Tx] {
  def prepend(elem: A)(implicit tx: S#Tx): Unit
  def append (elem: A)(implicit tx: S#Tx): Unit
  def remove (elem: A)(implicit tx: S#Tx): Boolean

  def apply(idx: Int)(implicit tx: S#Tx): A

  def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit

  def headOption(implicit tx: S#Tx): Option[A]

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  def size(implicit tx: S#Tx): Int

  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, A]

  def trimStart(n: Int)(implicit tx: S#Tx): Unit
  def trimEnd  (n: Int)(implicit tx: S#Tx): Unit
}