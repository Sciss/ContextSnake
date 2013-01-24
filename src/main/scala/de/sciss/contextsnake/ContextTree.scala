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

  trait Like[A] {
    def size: Int
    def length: Int
    def isEmpty: Boolean
    def nonEmpty: Boolean
    def +=(elem: A): this.type
    def append(elems: A*): Unit
    def appendAll(xs: TraversableOnce[A]): Unit
    def apply(idx: Int): A
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A]
  }

  trait Snake[A] extends Like[A] {
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

    /**
     * Appends a single element to the snake. Throws an exception if the element is
     * not a possible successor of the current body.
     *
     * @param elem the element to append
     */
    def +=(elem: A): this.type

    /**
     * Appends multiple elements to the snake. Throws an exception if the elements do
     * not form a valid growth path from the current body.
     *
     * @param elems the elements to append
     */
    def append(elems: A*): Unit

    /**
     * Appends all elements of a collection to the snake. Throws an exception if the elements do
     * not form a valid growth path from the current body.
     *
     * @param xs  the collection whose elements should be appended
     */
    def appendAll(xs: TraversableOnce[A]): Unit

    /**
     * Retrieves the element from the snake's body at a given position. If the position
     * is less than zero or greater than or equal to the snake's length, an exception is thrown.
     *
     * @param idx the index into the snake
     * @return  the element at the given index
     */
    def apply(idx: Int): A

    /**
     * Copies the snake's current body to a new independent collection.
     *
     * @param cbf   the builder factory for the target collection
     * @tparam Col  the type of the target collection
     * @return  the copied collection
     */
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A]
  }

  @elidable(INFO) private final val DEBUG = false
  @elidable(INFO) private def DEBUG_LOG(message: => String) {
    if (DEBUG) println(message)
  }

  private final class Impl[A] extends ContextTree[A] {
    private val corpus  = mutable.Buffer.empty[A]
    /* @elidable(INFO) */ private var nodeCount = 1 // scalac crashes when elidable
    @elidable(INFO) private def nextNodeID() = {
      val res = nodeCount
      nodeCount += 1
      res
    }

    private sealed trait Position {
      final var source: RootOrNode = RootNode
      final var startIdx: Int = 0
      def stopIdx: Int
//      final var stopIdx: Int = 0

//      def isExplicit  = startIdx >= stopIdx
//      def span        = stopIdx - startIdx

      def isExplicit: Boolean
      def span: Int

      final def dropToTail() {
        source match {
          case Node(tail) =>
            DEBUG_LOG("DROP: Suffix link from " + source + " to " + tail)
            source = tail
          case RootNode =>
            DEBUG_LOG("DROP: At root")
            startIdx += 1
        }
        canonize()
      }

      protected def reachedLeaf(): Unit

      final def canonize() {
        DEBUG_LOG(">>>> CANONIZE " + this)
        while (!isExplicit) {
          val edge       = source.edges(corpus(startIdx))
          val edgeSpan   = edge.span
          DEBUG_LOG("     edges(" + corpus(startIdx) + ") = " + edge)
          if (edgeSpan > span) {
            DEBUG_LOG("<<<< CANONIZE " + this + "\n")
            return
          }
          edge.targetNode match {
            case n: Node  =>
              source     = n
              startIdx  += edgeSpan
            case Leaf =>
              reachedLeaf()
              DEBUG_LOG("<<<< CANONIZE (LEAF) " + this)
              return
          }
          DEBUG_LOG("     now " + this)
        }
        DEBUG_LOG("<<<< CANONIZE " + this)
      }

      protected def prefix: String

      override def toString = prefix + "(start=" + startIdx + ", stop=" + stopIdx + {
          val num = span
          if (num > 0) {
            corpus.view(startIdx, math.min(stopIdx, startIdx + 4)).mkString(", seq=<", ",",
              if (num > 4) ",...," + corpus(stopIdx - 1) + ">" else ">")
          } else {
            ""
          }
        } + ", source=" + source + ")"
    }

    private final class Cursor extends Position {
//      private var edge: EdgeLike = DummyEdge
//      private var idx: Int = 0
      var stopIdx: Int = 0
//      private var edgeStopIdx: Int = 0
      private var exhausted = false

      def isExplicit  = startIdx >= stopIdx
//      def span        = idx - startIdx
      def span        = stopIdx - startIdx

//      @inline private def edgeExhausted = stopIdx == edgeStopIdx
//      private def edgeExhausted: Boolean = {
//        stopIdx == 0 || stopIdx >= source.edges(corpus(startIdx)).stopIdx
//      }

//      protected def prefix = "Cursor[" + idx + "]@" + hashCode().toHexString
      protected def prefix = "Cursor@" + hashCode().toHexString

//      def init(start: A): Boolean = initFromNode(RootNode, start)

      private def initFromNode(n: RootOrNode, elem: A): Boolean = {
        val edgeOption  = n.edges.get(elem)
        val found       = edgeOption.isDefined
        if (found) {
          val edge      = edgeOption.get
          source        = n
//          startIdx      = edge.startIdx
////          stopIdx       = edge.stopIdx
////          idx           = edge.startIdx + 1
          stopIdx       = edge.startIdx // will be incremented by tryMove!
          startIdx      = edge.startIdx // + 1
//          edgeStopIdx   = edge.stopIdx
        }
        found
      }

      protected def reachedLeaf() {
        exhausted = true
      }

//      private def sourceAfterExhausted : RootOrNodeOrLeaf = {
//        if (stopIdx == 0) {
//          RootNode
//        } else {
//          val edge = source.edges(corpus(startIdx))
//          edge.targetNode
//        }
//      }

      @inline private def implicitNext = corpus(stopIdx)
//      @inline private def implicitNext = corpus(startIdx)

      def tryMove(elem: A): Boolean = {
        val found = if (isExplicit) {
          initFromNode(source, elem)
        } else {
          !exhausted && implicitNext == elem
        }

        if (found) {
          stopIdx += 1
          canonize()
        }
        found
      }

//      def tryMove_(elem: A): Boolean = {
//        if (edgeExhausted) {
//          val prev = if (stopIdx == 0) RootNode else {
//            val prevEdge = source.edges(corpus(startIdx))
//            prevEdge.targetNode
//          }
//          prev match {
//            case n: RootOrNode => initFromNode(n, elem)
//            case Leaf => false
//          }
//        } else {  // implicit position
////        val found = corpus(idx) == elem
//          val found = corpus(stopIdx) == elem
////          if (found) idx += 1
//          if (found) stopIdx += 1
//          found
//        }
//      }

      def trimStart(): Boolean = {
//        val oldEdgeStart = startIdx
        dropToTail()
//        val delta = startIdx - oldEdgeStart
//val oldIdx = stopIdx // idx
//        idx += delta // ???
//        stopIdx += delta // ???
//println("TRIM START: old edge start was " + oldEdgeStart + "; new is " + startIdx ) // + "; stop was " + oldIdx + "; new is " + stopIdx )
        true
      }

//      def tryMove_(elem: A): Boolean = {
//        if (isExplicit) {
//          val edge = source.edges(corpus(startIdx))
//          edge.targetNode match {
//            case n: RootOrNode =>
//              val edgeOption  = n.edges.get(elem)
//              val found       = edgeOption.isDefined
//              if (found) {
////                edge          = edgeOption.get
//                source        = n
//                idx           = edge.startIdx + 1
//              }
//              found
//            case Leaf => false
//          }
//        } else {
//          val found = corpus(idx) == elem
//          if (found) idx += 1
//          found
//        }
//      }

//      def tryDropToTail(): Boolean = {
//        idx -= 1
//        if (idx > edge.startIdx) {
//          true
//        } else {
//          source match {
//            case Node(tail) =>
//              source  = tail
//              edge    = source.edges(corpus(idx))
//              idx     = edge.stopIdx // - 1
//              true
//            case RootNode => false
//          }
//        }
//      }

      def successors: Iterator[A] = {
        if (isExplicit) {
          source.edges.keysIterator
//          sourceAfterExhausted match {
//            case n: RootOrNode  => n.edges.keysIterator
//            case Leaf           => Iterator.empty
//          }
        } else if (exhausted) {
          Iterator.empty
        } else {
          Iterator.single(implicitNext)
        }
      }

//      def successors_ : Iterator[A] = {
//        if (isExplicit) {
//          val edge = source.edges(corpus(startIdx))
//          edge.targetNode match {
//            case n: RootOrNode =>
//              n.edges.keysIterator
//            case Leaf =>
//              Iterator.empty
//          }
//        } else {
////          Iterator.single(corpus(idx))
//          Iterator.single(corpus(stopIdx))
//        }
//      }

//      def successors_ : Iterator[A] = {
//        if (isExplicit) {
//          edge.targetNode match {
//            case n: RootOrNode =>
//              n.edges.keysIterator
//            case Leaf =>
//              Iterator.empty
//          }
//        } else {
//          Iterator.single(corpus(idx))
//        }
//      }
    }

    private final class SnakeImpl(body: mutable.Buffer[A], c: Cursor) extends Snake[A] {
      override def toString = "ContextTree.Snake(len=" + length +
        (if (length > 0) ", head=" + body.head + ", last=" + body.last else "") + ")@" + hashCode().toHexString // + "; csr=" + c

      def size: Int = body.length
      def length: Int = body.length
      def isEmpty: Boolean = body.isEmpty
      def nonEmpty: Boolean = body.nonEmpty

      def successors: Iterator[A] = c.successors

      def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = body.to[Col]
      def apply(idx: Int): A = body(idx)

      def trimEnd(n: Int) {
        ???
      }

      def trimStart(n: Int) {
        val sz  = size
        if (n > sz) throw new IndexOutOfBoundsException((n - sz).toString)
        var m   = 0
        while (m < n) {
          c.trimStart()
          m += 1
        }
        body.trimStart(n)
      }

      def appendAll(xs: TraversableOnce[A]) {
        xs.foreach(add1)
      }

      def append(elems: A*) { appendAll(elems) }

      def +=(elem: A): this.type = { snakeAdd1(elem); this }

      private def snakeAdd1(elem: A) {
        if (!c.tryMove(elem)) throw new NoSuchElementException(elem.toString)
        body += elem
      }
    }

    private object active extends Position /* (var node: RootOrNode, var startIdx: Int, var stopIdx: Int) */ {
      var stopIdx: Int = 0

      def isExplicit  = startIdx >= stopIdx
      def span        = stopIdx - startIdx

      def prefix = "active"

      protected def reachedLeaf() {
        assert(assertion = false)
      }
    }

    private sealed trait RootOrNodeOrLeaf
//    {
//      def getEdge(elem: A): Option[Edge]
//    }
    private sealed trait NodeOrLeaf extends RootOrNodeOrLeaf
    private sealed trait RootOrNode extends RootOrNodeOrLeaf {
      // use immutable.Set because we'll have many leave nodes,
      // and immutable.Set.empty is cheap compared to mutable.Set
      // ; another advantage is that we can return a view to
      // consumers of the tree without making a defensive copy
      final var edges = Map.empty[A, Edge]
//      final def getEdge(elem: A): Option[Edge] = edges.get(elem)
    }

//    private sealed trait Node extends NodeOrLeaf with RootOrNode {
//      @elidable(INFO) val id = nextNodeID()
//      @elidable(INFO) override def toString = id.toString
//    }

    private case object Leaf extends NodeOrLeaf
//    {
//      def getEdge(elem: A): Option[Edge] = None
//    }

    private object Node {
      def unapply(n: Node): Option[RootOrNode] = Some(n.tail)
    }
    private final class Node(var tail: RootOrNode) extends NodeOrLeaf with RootOrNode {
      @elidable(INFO) val id = nextNodeID()
      @elidable(INFO) override def toString = id.toString
    }

    private case object RootNode extends RootOrNode {
      override def toString = "0"
    }

    private sealed trait EdgeLike {
      def startIdx: Int
      def stopIdx: Int
      def span: Int
      def targetNode: RootOrNodeOrLeaf
    }

    private sealed trait Edge extends EdgeLike {
      def targetNode: NodeOrLeaf
      def replaceStart(newStart: Int): Edge
    }

//    private case object DummyEdge extends EdgeLike {
//      def startIdx: Int = 0
//      def stopIdx: Int = 0
//      def span: Int = 0
//      def targetNode: RootOrNodeOrLeaf = RootNode
//    }

    private final case class InnerEdge(startIdx: Int, stopIdx: Int, targetNode: Node) extends Edge {
      override def toString = "InnerEdge(start=" + startIdx + ", stop=" + stopIdx + ", target=" + targetNode + ")"
      def span = stopIdx - startIdx
      def replaceStart(newStart: Int) = copy(startIdx = newStart)
    }

    private final case class LeafEdge(startIdx: Int) extends Edge {
      override def toString = "LeafEdge(start=" + startIdx + ")"
      def targetNode: NodeOrLeaf = Leaf
      def stopIdx = corpus.length
      def span    = corpus.length - startIdx
      def replaceStart(newStart: Int) = copy(startIdx = newStart)
    }

    override def toString = "ContextTree(len=" + corpus.length + ")@" + hashCode().toHexString

    def snake(init: TraversableOnce[A]): Snake[A] = {
      val body  = init.toBuffer
      val c     = new Cursor
      if (!init.forall(c.tryMove)) throw new NoSuchElementException(init.toString)

      new SnakeImpl(body, c)
    }

    def contains(elem: A): Boolean = RootNode.edges.contains(elem)

    def containsSlice(xs: TraversableOnce[A]): Boolean = {
      val c = new Cursor
      xs.forall(c.tryMove)
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
            case n: Node =>
              sb.append(n.toString + " [label=\"" + str + "\"];\n")
              appendNode(n)
          }
        }

        if (tailEdges) source match {
          case Node(tail) =>
            val target = tail
            sb.append("  " + source + " -> " + target + " [style=dotted];\n")
          case RootNode =>
        }
      }
      appendNode(RootNode)

      sb.append("}\n")
      sb.toString
    }

    @inline private def split(edge: Edge): Node = {
      val startIdx    = edge.startIdx
      val startElem   = corpus(startIdx)
      val splitIdx    = startIdx + active.span
      val newNode     = new Node(active.source)
      val newEdge1    = InnerEdge(startIdx, splitIdx, newNode)
      active.source.edges += ((startElem, newEdge1))
      val newEdge2    = edge.replaceStart(splitIdx)
      newNode.edges  += ((corpus(splitIdx), newEdge2))
      DEBUG_LOG("SPLIT: " + edge + " -> new1 = " + newEdge1 + "; new2 = " + newEdge2)
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

      DEBUG_LOG("ADD: elem=" + elem + "; " + active)

      def addLink(n: RootOrNode, parent: RootOrNode) {
        n match {
          case n: Node =>
            DEBUG_LOG("LINK: from " + n + " to " + parent)
            n.tail = parent
          case RootNode =>
        }
      }

      @tailrec def loop(prev: RootOrNode): RootOrNode = {
        val parent = if (active.isExplicit) {
          if (active.source.edges.contains(elem)) return prev
          active.source
        } else {
          val edge = active.source.edges(corpus(active.startIdx))
          if (corpus(edge.startIdx + active.span) == elem) return prev
          split(edge)
        }

        // create new leaf edge starting at parentNode
        val newEdge = LeafEdge(elemIdx)
        parent.edges += ((elem, newEdge))
        addLink(prev, parent)

        // drop to tail suffix
        active.dropToTail()

        loop(parent)
      }

      val last = loop(RootNode)
      addLink(last, active.source)
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
trait ContextTree[A] extends ContextTree.Like[A] {
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