/*
 *  ContextTree.scala
 *  (ContextTree)
 *
 *  Copyright (c) 2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.contextsnake

import collection.{SeqView, mutable}
import annotation.{elidable, tailrec}
import elidable.INFO

object ContextTree {
  def empty[A]: ContextTree[A] = new Impl[A]

  def apply[A](elem: A*): ContextTree[A] = {
    val res = empty[A]
    res.appendAll(elem)
    res
  }

  private final class Impl[A] extends ContextTree[A] {
    private val corpus            = mutable.Buffer.empty[A]
    private var activeNode: RootOrNode = RootNode
    private var activeStartIdx    = 0
    private var activeStopIdx     = 0

    @elidable(INFO) private var nodeCount = 1
    @elidable(INFO) private def nextNodeID() = {
      val res = nodeCount
      nodeCount += 1
      res
    }

    private sealed trait RootOrNodeOrLeaf {
      def getEdge(elem: A): Option[Edge]
    }
    private sealed trait NodeOrLeaf extends RootOrNodeOrLeaf
    private sealed trait RootOrNode extends RootOrNodeOrLeaf {
      // use immutable.Set because we'll have many leave nodes,
      // and immutable.Set.empty is cheap compared to mutable.Set
      // ; another advantage is that we can return a view to
      // consumers of the tree without making a defensive copy
      final var edges = Map.empty[A, Edge]
      final def getEdge(elem: A): Option[Edge] = edges.get(elem)
      def dropTail(): Unit
    }

    private sealed trait Node extends NodeOrLeaf with RootOrNode {
      @elidable(INFO) val id = nextNodeID()
      @elidable(INFO) override def toString = id.toString
    }

    private case object Leaf extends NodeOrLeaf {
      def getEdge(elem: A): Option[Edge] = None
    }

    private final class InnerNode(var tail: RootOrNode) extends Node {
      def dropTail() {
        activeNode  = tail
        canonize()
      }
    }

    private case object RootNode extends RootOrNode {
      override def toString = "0"
      def dropTail() {
        activeStartIdx += 1
      }
    }

    private sealed trait Edge {
      def startIdx: Int
      def stopIdx: Int
      def span: Int
      def targetNode: NodeOrLeaf
      def replaceStart(newStart: Int): Edge
    }

    private final case class InnerEdge(startIdx: Int, stopIdx: Int, targetNode: InnerNode) extends Edge {
      def span = stopIdx - startIdx
      def replaceStart(newStart: Int) = copy(startIdx = newStart)
    }

    private final case class LeafEdge(startIdx: Int) extends Edge {
      def targetNode: NodeOrLeaf = Leaf
      def stopIdx = corpus.length
      def span    = corpus.length - startIdx
      def replaceStart(newStart: Int) = copy(startIdx = newStart)
    }

    override def toString = "ContextTree(len=" + corpus.length + ")@" + hashCode().toHexString

    def contains(elem: A): Boolean = RootNode.edges.contains(elem)

    def containsSlice(xs: TraversableOnce[A]): Boolean = {
      var n: RootOrNodeOrLeaf = RootNode
      var start = 0
      var stop  = 0
      xs.foreach { e =>
        if (start < stop) {
          if (corpus(start) != e) return false  // not found in implicit node
          start += 1
        } else n.getEdge(e) match {
          case None       => return false       // reached end of leaf node
          case Some(edge) =>
            n     = edge.targetNode
            start = edge.startIdx + 1
            stop  = edge.stopIdx
        }
      }
      true
    }

    def size: Int = corpus.length
    def apply(idx: Int): A = corpus(idx)
    def view(from: Int, until: Int): SeqView[A, mutable.Buffer[A]] = corpus.view(from, until)
    def isEmpty: Boolean = corpus.isEmpty
    def nonEmpty: Boolean = corpus.nonEmpty

    def toDOT(tailEdges: Boolean, sep: String): String = {
      val sb = new StringBuffer()
      sb.append("digraph suffixes {\n")

      var leafCnt = 0

      def appendNode(source: RootOrNode) {
        sb.append("  " + source + " [shape=circle];\n")
        source.edges.foreach { case (_, edge) =>
          val str     = corpus.slice(edge.startIdx, edge.stopIdx).mkString(sep)
          sb.append("  " + source + " -> ")
          edge.targetNode match {
            case Leaf =>
              val t = "leaf" + leafCnt
              leafCnt += 1
              sb.append( t + " [label=\"" + str + "\"];\n")
              sb.append( "  " + t + " [shape=point];\n")
            case i: InnerNode =>
              sb.append(i.toString + " [label=\"" + str + "\"];\n")
              appendNode(i)
          }
        }

        if (tailEdges) source match {
          case i: InnerNode =>
            val target = i.tail
            sb.append("  " + source + " -> " + target + " [style=dotted];\n")
          case _ =>
        }
      }
      appendNode(RootNode)

      sb.append("}\n")
      sb.toString
    }

    @inline private def isExplicit  = activeStartIdx >= activeStopIdx
    @inline private def activeSpan  = activeStopIdx - activeStartIdx

    @inline private def split(edge: Edge): InnerNode = {
      val startIdx    = edge.startIdx
      val startElem   = corpus(startIdx)
      val splitIdx    = startIdx + activeSpan
      val newNode     = new InnerNode(activeNode)
      val newEdge1    = InnerEdge(startIdx, splitIdx, newNode)
      activeNode.edges += ((startElem, newEdge1))
      val newEdge2    = edge.replaceStart(splitIdx)
      newNode.edges += ((corpus(splitIdx), newEdge2))
      newNode
    }

    @inline private def canonize() {
      while (!isExplicit) {
        val edge        = activeNode.edges(corpus(activeStartIdx))
        val edgeSpan    = edge.span
        if (edgeSpan > activeSpan) return
        activeStartIdx += edgeSpan
        activeNode      = edge.targetNode.asInstanceOf[Node]    // TODO shouldn't need a cast -- how to proof this cannot be Leaf?
      }
    }

    def +=(elem: A): this.type = { add1(elem); this }

    def append(elem: A*) {
      elem foreach add1
    }

    def appendAll(xs: TraversableOnce[A]) {
      xs foreach add1
    }

    private def add1(elem: A) {
      val elemIdx     = corpus.length
      corpus         += elem

      @tailrec def loop(prev: RootOrNode) {
        val parent = if (isExplicit) {
          if (activeNode.edges.contains(elem)) return
          activeNode
        } else {
          val edge = activeNode.edges(corpus(activeStartIdx))
          if (corpus(edge.startIdx + activeSpan) == elem) return
          split(edge)
        }

        // create new leaf edge starting at parentNode
        val newEdge = LeafEdge(elemIdx)
        parent.edges += ((elem, newEdge))
        prev match {
          case i: InnerNode => i.tail = parent
          case _ =>
        }

        // drop to tail suffix
        activeNode.dropTail()

        loop(parent)
      }

      loop(RootNode)
      activeStopIdx += 1
      canonize()
    }
  }
}

/**
 * A mutable data append-only structure that support efficient searching for sub-sequences.
 * In this version, it is just a suffix tree.
 *
 * @tparam A  the element type of the structure
 */
trait ContextTree[A] {
  /**
   * Appends an element to the tree.
   *
   * @param elem  the element to append
   * @return      this same tree
   */
  def +=(elem: A): this.type

  /**
   * Appends multiple elements to the tree
   *
   * @param elems the elements to append
   */
  def append(elems: A*): Unit

  /**
   * Appends all elements of a collection to the tree. The elements are
   * appended in the order in which they are contained in the argument.
   *
   * @param xs  the collection whose elements should be appended
   */
  def appendAll(xs: TraversableOnce[A]): Unit

  /**
   * Tests whether a given sub-sequence is contained in the tree.
   * This is a very fast operation taking O(|xs|).
   *
   * @param xs  the sequence to look for
   * @return    `true` if the sequence is included in the tree, `false` otherwise
   */
  def containsSlice(xs: TraversableOnce[A]): Boolean

  /**
   * Tests whether an element is contained in the tree.
   * This is a constant time operation.
   *
   * @param elem  the element to look for
   * @return    `true` if the element is included in the tree, `false` otherwise
   */
  def contains(elem: A): Boolean
  
//  def snake(init: TraversableOnce[A])

  /**
   * Queries the number of elements in the tree
   */
  def size: Int

  /**
   * Queries whether the collection is empty (has zero elements)
   */
  def isEmpty: Boolean

  /**
   * Queries whether the collection non-empty (has one or more elements)
   */
  def nonEmpty: Boolean

  /**
   * Queries an element at a given index. Throws an exception if the `idx` argument
   * is negative or greater than or equal to the size of the tree.
   *
   * @param idx the index of the element
   * @return  the element at the given index
   */
  def apply(idx: Int): A

  /**
   * Provides a view of a range of the underlying buffer. Technically, because
   * the underlying buffer is mutable, this view would be subject to mutations as well until
   * a copy is built. However, since the tree is append-only, the portion visible
   * in the view will never change.
   *
   * Note that, like the `view` method in `collection.mutable.Buffer`, the range is
   * clipped to the length of the underlying buffer _at this moment_. For example,
   * if the buffer currently has 6 elements, a `view(7,8)` is treated as `view(6,6)`
   * and will always be empty. Therefore it is save to treat the view as immutable.
   *
   * @param from  the start index into the collection
   * @param until the stop index (exclusive) into the collection
   *
   * @return  a view of the given range.
   */
  def view(from: Int, until: Int): SeqView[A, mutable.Buffer[A]]

  /**
   * Helper method to export the tree to GraphViz DOT format.
   * This is mostly for debugging or demonstration purposes and might not be
   * particularly efficient or suitable for large trees.
   *
   * @param tailEdges whether to include the tail (suffix-pointer) edges or not
   * @return  a string representation in DOT format
   */
  def toDOT(tailEdges: Boolean = false, sep: String = ""): String
}