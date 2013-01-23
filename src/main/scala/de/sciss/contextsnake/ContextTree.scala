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
import collection.generic.CanBuildFrom

object ContextTree {
  def empty[A]: ContextTree[A] = new Impl[A]

  def apply[A](elem: A*): ContextTree[A] = {
    val res = empty[A]
    res.appendAll(elem)
    res
  }

  trait Snake[A] {
    /**
     * The size of the snake. Same as `length`
     */
    def size: Int

    /**
     * The number of elements in the snake.
     */
    def length: Int

    /**
     * Removes the last `n` elements in the snake.
     * Throws an exception if `n` is greater than `length`.
     *
     * @param n the number of elements to drop from the end
     */
    def trimEnd(n: Int): Unit

    /**
     * Removes the first `n` elements in the snake.
     * Throws an exception if `n` is greater than `length`.
     *
     * @param n the number of elements to drop from the beginning
     */
    def trimStart(n: Int): Unit

    def successors: Iterator[A]
  }

  private final class Impl[A] extends ContextTree[A] {
    private val corpus  = mutable.Buffer.empty[A]
//    private val active  = new Cursor // (RootNode, 0, 0)
//    private var activeNode: RootOrNode = RootNode
//    private var activeStartIdx    = 0
//    private var activeStopIdx     = 0

    @elidable(INFO) private var nodeCount = 1
    @elidable(INFO) private def nextNodeID() = {
      val res = nodeCount
      nodeCount += 1
      res
    }

    private sealed trait Position {
      final var startIdx: Int = 0
      final var stopIdx: Int = 0

      final def isExplicit  = startIdx >= stopIdx
      final def span        = stopIdx - startIdx
    }

    private final class Cursor extends Position {
      var target: RootOrNodeOrLeaf = RootNode
    }

    private final class SnakeImpl(body: mutable.Buffer[A], c: Cursor) extends Snake[A] {
      override def toString = "ContextTree.Snake(len=" + length +
        (if (length > 0) ", head=" + body.head + ", last=" + body.last else "") + ")@" + hashCode().toHexString

      def size: Int = body.length
      def length: Int = body.length
      def trimEnd(n: Int) { ??? }
      def trimStart(n: Int) { ??? }
      def successors: Iterator[A] = {
        if (c.isExplicit) {
          c.target match {
            case n: RootOrNode =>
              n.edges.keysIterator
            case Leaf =>
              Iterator.empty
          }
        } else {
          Iterator.single(corpus(c.startIdx))
        }
      }
    }

    private object active extends Position /* (var node: RootOrNode, var startIdx: Int, var stopIdx: Int) */ {
      var source: RootOrNode = RootNode

      def dropToTail() {
        source match {
          case i: InnerNode =>
            source = i.tail
            canonize()
          case RootNode =>
            startIdx += 1
        }
      }

      def canonize() {
        while (!isExplicit) {
          val edge       = source.edges(corpus(startIdx))
          val edgeSpan   = edge.span
          if (edgeSpan > span) return
          startIdx      += edgeSpan
          source         = edge.targetNode.asInstanceOf[Node]    // TODO shouldn't need a cast -- how to proof this cannot be Leaf?
        }
      }
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
    }

    private sealed trait Node extends NodeOrLeaf with RootOrNode {
      @elidable(INFO) val id = nextNodeID()
      @elidable(INFO) override def toString = id.toString
    }

    private case object Leaf extends NodeOrLeaf {
      def getEdge(elem: A): Option[Edge] = None
    }

    private final class InnerNode(var tail: RootOrNode) extends Node

    private case object RootNode extends RootOrNode {
      override def toString = "0"
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

    def snake(init: TraversableOnce[A]): Snake[A] = {
      val body  = init.toBuffer
      val c     = new Cursor
      if (!initCursor(c, init)) throw new NoSuchElementException(init.toString)

      new SnakeImpl(body, c)
    }

    def contains(elem: A): Boolean = RootNode.edges.contains(elem)

    def containsSlice(xs: TraversableOnce[A]): Boolean = {
      initCursor(new Cursor, xs)
    }

    private def initCursor(c: Cursor, xs: TraversableOnce[A]): Boolean = {
      xs.foreach { e =>
        if (c.startIdx < c.stopIdx) {
          if (corpus(c.startIdx) != e) return false // not found in implicit node
          c.startIdx += 1
        } else c.target.getEdge(e) match {
          case None       => return false           // reached end of leaf node
          case Some(edge) =>
            c.target    = edge.targetNode
            c.startIdx  = edge.startIdx + 1
            c.stopIdx   = edge.stopIdx
        }
      }
      true
    }

    def size: Int = corpus.length
    def length: Int = corpus.length
    def isEmpty: Boolean = corpus.isEmpty
    def nonEmpty: Boolean = corpus.nonEmpty
    def apply(idx: Int): A = corpus(idx)
    def view(from: Int, until: Int): SeqView[A, mutable.Buffer[A]] = corpus.view(from, until)
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = corpus.to(cbf)

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
          case RootNode =>
        }
      }
      appendNode(RootNode)

      sb.append("}\n")
      sb.toString
    }

    @inline private def split(edge: Edge): InnerNode = {
      val startIdx    = edge.startIdx
      val startElem   = corpus(startIdx)
      val splitIdx    = startIdx + active.span
      val newNode     = new InnerNode(active.source)
      val newEdge1    = InnerEdge(startIdx, splitIdx, newNode)
      active.source.edges += ((startElem, newEdge1))
      val newEdge2    = edge.replaceStart(splitIdx)
      newNode.edges += ((corpus(splitIdx), newEdge2))
      newNode
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
        val parent = if (active.isExplicit) {
          if (active.source.edges.contains(elem)) return
          active.source
        } else {
          val edge = active.source.edges(corpus(active.startIdx))
          if (corpus(edge.startIdx + active.span) == elem) return
          split(edge)
        }

        // create new leaf edge starting at parentNode
        val newEdge = LeafEdge(elemIdx)
        parent.edges += ((elem, newEdge))
        prev match {
          case i: InnerNode => i.tail = parent
          case RootNode =>
        }

        // drop to tail suffix
        active.dropToTail()

        loop(parent)
      }

      loop(RootNode)
      active.stopIdx += 1
      active.canonize()
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
  
//  def indexOfSlice(xs: TraversableOnce[A]): Int

  /**
   * Creates a new snake through the tree from a given initial sequence.
   * This initial sequence must be contained in the tree (e.g. `containsSlice` must return `true`),
   * otherwise an exception is thrown.
   *
   * To construct a snake from a particular index range of the tree, use
   * `snake(view(from, until))`. Note that because the sequence might occur multiple
   * times in the corpus, this does not guarantee any particular resulting index
   * into the tree.
   *
   * @param init  the sequence to begin with
   * @return  a new snake whose content is `init`
   */
  def snake(init: TraversableOnce[A]): ContextTree.Snake[A]

  /**
   * Queries the number of elements in the tree
   */
  def size: Int

  /**
   * The length of the collection in this tree. Same as `size`
   */
  def length: Int

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
   * Converts this tree into another collection by copying all elements.
   *
   * @param cbf   the builder factory which determines the target collection type
   * @tparam Col  the target collection type
   * @return  a new independent collection containing all elements of this tree
   */
  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A]

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