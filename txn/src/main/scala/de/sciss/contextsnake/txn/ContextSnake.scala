/*
 *  ContextTree.scala
 *  (ContextTree)
 *
 *  Copyright (c) 2013-2014 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.contextsnake.txn

import collection.{SeqView, mutable}
import scala.annotation.{switch, elidable, tailrec}
import elidable.INFO
import collection.generic.CanBuildFrom
import de.sciss.lucre.stm.{Mutable, Sys}
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.Space
import language.higherKinds
import de.sciss.lucre.data
import de.sciss.serial.{Writable, DataInput, DataOutput, Serializer}

object ContextTree {
  /** Creates a new empty context tree for a given element type.
    * Elements can then be added using `+=`, `append`, or `appendAll`.
    *
    * @tparam A  the element type
    */
  def empty[S <: Sys[S], D <: Space[D], A](hyperCube: D#HyperCube)
                                          (implicit tx: S#Tx, pointView: A => D#PointLike, space: D,
                                           elemSerializer: Serializer[S#Tx, S#Acc, A]): ContextTree[S, A] = {
    new Impl[S, D, A] {
      val id: S#ID = tx.newID()

      val rootEdges = SkipOctree.empty[S, D, (A, Edge)](hyperCube)(
        tx, entryPointView, space, EntrySerializer)

      val activeStartIdx: S#Var[Int] = tx.newVar(id, 0)
      val activeStopIdx : S#Var[Int] = tx.newVar(id, 0)

      val activeSource  : S#Var[RootOrNode] = tx.newVar(id, RootNode: RootOrNode)(RootOrNodeSerializer)

    }
  }

  /** Creates a context tree populated with the given elements.
    *
    * @param elem  the elements to add in their original order
    * @tparam A    the element type
    */
  def apply[S <: Sys[S], D <: Space[D], A](elem: A*)(hyperCube: D#HyperCube)
                                          (implicit tx: S#Tx, pointView: A => D#PointLike, space: D,
                                           elemSerializer: Serializer[S#Tx, S#Acc, A]): ContextTree[S, A] = {
    val res = empty[S, D, A](hyperCube)
    res.appendAll(elem)
    res
  }

  /** A common trait to the suffix tree and navigating snakes.
    * Since they are backed by a `collection.mutable.Buffer`, most operations
    * exposed here use the buffer terminology.
    *
    * @tparam A  the element type of the tree/snake
    */
  trait Like[S <: Sys[S], A] {
    def size: Int
    def length: Int
    def isEmpty: Boolean
    def nonEmpty: Boolean
    def +=(elem: A)(implicit tx: S#Tx): this.type
    def append(elems: A*)(implicit tx: S#Tx): Unit
    def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit
    def apply(idx: Int): A
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A]
  }

  /** A `Snake` represents a sliding window over a context tree's corpus. */
  trait Snake[S <: Sys[S], A] extends Like[S, A] {
    /** The size of the snake. Same as `length`. */
    def size: Int

    /** The number of elements in the snake. */
    def length: Int

    /** Removes the last `n` elements in the snake.
      * Throws an exception if `n` is greater than `length`.
      *
      * @param n the number of elements to drop from the end
      */
    def trimEnd(n: Int)(implicit tx: S#Tx): Unit

    /** Removes the first `n` elements in the snake.
      * Throws an exception if `n` is greater than `length`.
      *
      * @param n the number of elements to drop from the beginning
      */
    def trimStart(n: Int)(implicit tx: S#Tx): Unit

    def successors(implicit tx: S#Tx): data.Iterator[S#Tx, A]

    /** Appends a single element to the snake. Throws an exception if the element is
      * not a possible successor of the current body.
      *
      * @param elem the element to append
      */
    def +=(elem: A)(implicit tx: S#Tx): this.type

    /** Appends multiple elements to the snake. Throws an exception if the elements do
      * not form a valid growth path from the current body.
      *
      * @param elems the elements to append
      */
    def append(elems: A*)(implicit tx: S#Tx): Unit

    /** Appends all elements of a collection to the snake. Throws an exception if the elements do
      * not form a valid growth path from the current body.
      *
      * @param xs  the collection whose elements should be appended
      */
    def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit

    /** Retrieves the element from the snake's body at a given position. If the position
      * is less than zero or greater than or equal to the snake's length, an exception is thrown.
      *
      * @param idx the index into the snake
      * @return  the element at the given index
      */
    def apply(idx: Int): A

    /** Copies the snake's current body to a new independent collection.
      *
      * @param cbf   the builder factory for the target collection
      * @tparam Col  the type of the target collection
      * @return  the copied collection
      */
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A]
  }

  @elidable(INFO) private final val DEBUG = false
  @elidable(INFO) private def DEBUG_LOG(message: => String): Unit =
    if (DEBUG) println(message)

  private abstract class Impl[S <: Sys[S], D <: Space[D], A](implicit pointView: A => D#PointLike, space: D,
      elemSerializer: Serializer[S#Tx, S#Acc, A])
    extends ContextTree[S, A] with Mutable.Impl[S] {

    // ---- abstract ----

    protected def rootEdges: SkipOctree[S, D, (A, Edge)]
    protected def activeStartIdx: S#Var[Int]
    protected def activeStopIdx : S#Var[Int]
    protected def activeSource  : S#Var[RootOrNode]

    // ---- implemented ----

    final protected def hyperCube: D#HyperCube = rootEdges.hyperCube

    final implicit protected def pointViewTx(elem: A, tx: S#Tx): D#PointLike = pointView(elem)
    final implicit protected def entryPointView(tup: (A, Edge), tx: S#Tx): D#PointLike = pointView(tup._1)

    val corpus = mutable.Buffer.empty[A]

    //    private var nodeCount = 1 // note: scalac crashes when this is marked `elidable`
    //    @elidable(INFO) def nextNodeID() = {
    //      val res = nodeCount
    //      nodeCount += 1
    //      res
    //    }

    protected def writeData(out: DataOutput): Unit = {
      rootEdges     .write(out)
      activeStartIdx.write(out)
      activeStopIdx .write(out)
      activeSource  .write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      rootEdges     .dispose()
      activeStartIdx.dispose()
      activeStopIdx .dispose()
      activeSource  .dispose()
    }

    implicit protected object EntrySerializer extends Serializer[S#Tx, S#Acc, (A, Edge)] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): (A, Edge) = {
        val elem = elemSerializer.read(in, access)
        val edge = (in.readByte(): @switch) match {
          case 0 => // InnerEdge
            val startIdx    = in.readInt()
            val stopIdx     = in.readInt()
            val targetNode  = Node.read(in, access)
            new InnerEdge(startIdx = startIdx, stopIdx = stopIdx, targetNode = targetNode)

          case 1 => // LeafEdge
            val startIdx = in.readInt()
            new LeafEdge(startIdx)

          case other => sys.error(s"Unexpected cookie ($other)")
        }
        (elem, edge)
      }

      def write(v: (A, Edge), out: DataOutput): Unit = {
        elemSerializer.write(v._1, out)
        v._2.write(out)
      }
    }

    sealed trait Position {
      // final var source: RootOrNode = RootNode

      def source: S#Var[RootOrNode]

      // final var startIdx: Int = 0
      // final var stopIdx:  Int = 0

      def startIdx: S#Var[Int]
      def stopIdx : S#Var[Int]

      final def isExplicit(implicit tx: S#Tx)  = startIdx() >= stopIdx()
      final def span      (implicit tx: S#Tx)  = stopIdx() - startIdx()

      final def dropToTail()(implicit tx: S#Tx): Unit = {
        source() match {
          case Node(tail) =>
            DEBUG_LOG(s"DROP: Suffix link from $source to $tail")
            source() = tail
          case RootNode =>
            DEBUG_LOG("DROP: At root")
            startIdx.transform(_ + 1)
        }
        canonize()
      }

      // called when during canonisation we drop to a leaf node.
      // this can mean an assertion error or a particular condition
      // such as signalising that a cursor is exhausted
      def reachedLeaf()(implicit tx: S#Tx): Unit

      /*
       * This method should be called whenever the position is moved. It will
       * normalise the position. If the position denotes an implicit node
       * and the offset shoots past that node's end, we drop to the edge's
       * target node and repeat the check.
       */
      final def canonize()(implicit tx: S#Tx): Unit = {
        DEBUG_LOG(s">>>> CANONIZE $this")
        while (!isExplicit) {
          val startElem   = corpus(startIdx())
          val startPoint  = pointView(startElem)
          val edge        = source().edges.get(startPoint)
            .getOrElse(throw new NoSuchElementException(startElem.toString))._2
          val edgeSpan    = edge.span
          DEBUG_LOG(s"     edges($startElem) = $edge")
          if (edgeSpan > span) {
            DEBUG_LOG(s"<<<< CANONIZE $this\n")
            return
          }
          edge.targetNode match {
            case n: Node  =>
              source()   = n
              startIdx.transform(_ + edgeSpan)
            case Leaf =>
              reachedLeaf()
              DEBUG_LOG(s"<<<< CANONIZE (LEAF) $this")
              return
          }
          DEBUG_LOG(s"     now $this")
        }
        DEBUG_LOG(s"<<<< CANONIZE $this")
      }

      /*
       * The prefix is used to distinguish different instances of `Position` in `toString`
       */
      def prefix: String

      override def toString = {
        // val num = span
        val seqInfo =
          // if (num > 0) {
        //   corpus.view(startIdx, math.min(stopIdx, startIdx + 4)).mkString(", seq=<", ",",
        //     if (num > 4) ",...," + corpus(stopIdx - 1) + ">" else ">")
        // } else {
            ""
        // }
        s"$prefix(start=$startIdx, stop=$stopIdx$seqInfo, source=$source)"
      }
    }

    final class Cursor(val id: S#ID, val source: S#Var[RootOrNode],
                       val startIdx: S#Var[Int],
                       val stopIdx : S#Var[Int], exhausted: S#Var[Boolean])
      extends Position with Mutable.Impl[S] {

      // private var exhausted = false // when the cursor's last position has come to the very end of the corpus

      def prefix = s"Cursor@${hashCode().toHexString}"

      protected def writeData(out: DataOutput): Unit = {
        startIdx .write(out)
        stopIdx  .write(out)
        exhausted.write(out)
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = {
        startIdx .dispose()
        stopIdx  .dispose()
        exhausted.dispose()
      }

      private def initFromNode(n: RootOrNode, elem: A)(implicit tx: S#Tx): Boolean = {
        val edgeOption  = n.edges.get(elem)
        val found       = edgeOption.isDefined
        if (found) {
          val edge      = edgeOption.get._2
          source()      = n
          stopIdx()     = edge.startIdx // will be incremented by tryMove!
          startIdx()    = edge.startIdx
        }
        found
      }

      // sets the `exhausted` flag which is used in `tryMove` and `successors`
      def reachedLeaf()(implicit tx: S#Tx): Unit =
        exhausted() = true

      // the next element, assuming we are on an implicit node
      private def implicitNext(implicit tx: S#Tx) = corpus(stopIdx())

      /** Tries to move the cursor one position forward by selecting the given element.
        *
        * @param elem  the element to follow to
        * @return      `true` if the element was a possible successor, `false` if not (this aborts the move)
        */
      def tryMove(elem: A)(implicit tx: S#Tx): Boolean = {
        val found = if (isExplicit) {
          initFromNode(source(), elem)
        } else {
          !exhausted() && implicitNext == elem
        }

        if (found) {
          stopIdx.transform(_ + 1)
          canonize()
        }
        found
      }

      /** Drops the first element in the suffix. */
      def trimStart()(implicit tx: S#Tx): Unit = dropToTail()

      /** Drops the last element in the suffix. */
      def trimEnd()(implicit tx: S#Tx): Unit = {
        if (isExplicit) {
          source() match {
            case n: Node =>
              val parent      = n.init()
              val startElem   = corpus(startIdx())
              val startPoint  = pointView(startElem)
              val edge        = parent.edges.get(startPoint)
                .getOrElse(throw new NoSuchElementException(startElem.toString))._2
              source()        = parent
              stopIdx()       = edge.stopIdx - 1
              startIdx()      = edge.startIdx

            case RootNode =>
              throw new UnsupportedOperationException("trimEnd on the beginning of the corpus")
          }
        } else {
          stopIdx.transform(_ - 1)
        }
        // if (exhausted) exhausted = false
        exhausted() = false
      }

      /** Queries the possible successor elements of the current suffix
        *
        * @return  an iterator over the possible elements (any of which can be safely passed to `tryMove`).
        *          This will be empty if the cursor is exhausted. It will be `1` if the cursor is currently on
        *          and implicit node.
        */
      def successors(implicit tx: S#Tx): data.Iterator[S#Tx, A] =
        if (isExplicit) {
          source().edges.iterator.map(_._1)
        } else if (exhausted()) {
          data.Iterator.empty
        } else {
          data.Iterator.wrap(Iterator.single(implicitNext))
        }
    }

    private final class SnakeImpl(body: mutable.Buffer[A], c: Cursor) extends Snake[S, A] {
      override def toString = {
        val headInfo = if (length > 0) s", head=${body.head}, last=${body.last}" else ""
        s"ContextTree.Snake(len=$length$headInfo)@${hashCode().toHexString}" // + "; csr=" + c
      }

      def size: Int         = body.length
      def length: Int       = body.length
      def isEmpty: Boolean  = body.isEmpty
      def nonEmpty: Boolean = body.nonEmpty

      def successors(implicit tx: S#Tx): data.Iterator[S#Tx, A] = c.successors

      def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = body.to[Col]
      def apply(idx: Int): A = body(idx)

      def trimEnd(n: Int)(implicit tx: S#Tx): Unit = {
        if (n > size) throw new IndexOutOfBoundsException((n - size).toString)
        var m = 0
        while (m < n) {
          c.trimEnd()
          m += 1
        }
        body.trimEnd(n)
      }

      def trimStart(n: Int)(implicit tx: S#Tx): Unit = {
        if (n > size) throw new IndexOutOfBoundsException((n - size).toString)
        var m = 0
        while (m < n) {
          c.trimStart()
          m += 1
        }
        body.trimStart(n)
      }

      def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit = xs.foreach(add1)

      def append(elems: A*)(implicit tx: S#Tx): Unit = appendAll(elems)

      def +=(elem: A)(implicit tx: S#Tx): this.type = {
        snakeAdd1(elem)
        this
      }

      private def snakeAdd1(elem: A)(implicit tx: S#Tx): Unit = {
        if (!c.tryMove(elem)) throw new NoSuchElementException(elem.toString)
        body += elem
      }
    }

    private object active extends Position /* (var node: RootOrNode, var startIdx: Int, var stopIdx: Int) */ {
      def prefix = "active"

      def startIdx: S#Var[Int]        = activeStartIdx
      def stopIdx : S#Var[Int]        = activeStopIdx
      def source  : S#Var[RootOrNode] = activeSource

      // the active point should never reach a leaf
      def reachedLeaf()(implicit tx: S#Tx): Unit =
        assert(assertion = false)
    }

    /*
     * Any node in the tree, either the root, an inner node, or a leaf
     */
    sealed trait RootOrNodeOrLeaf
    /*
     * An inner node or a leaf, but not the root
     */
    sealed trait NodeOrLeaf extends RootOrNodeOrLeaf

    implicit object RootOrNodeSerializer extends Serializer[S#Tx, S#Acc, RootOrNode] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): RootOrNode = (in.readByte(): @switch) match {
        case 0 => RootNode
        case 1 => Node.readIdentified(in, access)
        case other => sys.error(s"Unexpected cookie ($other)")
      }

      def write(v: RootOrNode, out: DataOutput): Unit = v.write(out)
    }

    /*
     * The root or an inner node, but not a leaf
     */
    sealed trait RootOrNode extends RootOrNodeOrLeaf with Writable {
      // use immutable.Set because we'll have many leave nodes,
      // and immutable.Set.empty is cheap compared to mutable.Set
      // ; another advantage is that we can return a view to
      // consumers of the tree without making a defensive copy

      // final var edges = Map.empty[A, Edge]

      def edges: SkipOctree[S, D, (A, Edge)]
    }

    case object Leaf extends NodeOrLeaf

    object Node {
      // extracts the tail parameter
      def unapply(n: Node)(implicit tx: S#Tx): Option[RootOrNode] = Some(n.tail())

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Node = {
        val cookie = in.readByte()
        require(cookie == 1, s"Unexpected cookie (found $cookie, expected 1)")
        readIdentified(in, access)
      }

      def readIdentified(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Node = {
        val id      = tx.readID(in, access)
        val parent  = RootOrNodeSerializer.read(in, access)
        val edges   = SkipOctree.read[S, D, (A, Edge)](in, access)
        val tail    = tx.readVar[RootOrNode](id, in)
        val init    = tx.readVar[RootOrNode](id, in)
        new Node(id, parent, edges, tail = tail, init = init)
      }
    }
    final class Node(val id: S#ID, parent: RootOrNode, val edges: SkipOctree[S, D, (A, Edge)],
                     val tail: S#Var[RootOrNode], val init: S#Var[RootOrNode])
      extends NodeOrLeaf with RootOrNode with Mutable[S#ID, S#Tx] {

      def write(out: DataOutput): Unit = {
        out.writeByte(1)
        id    .write(out)
        parent.write(out)
        edges .write(out)
        tail  .write(out)
        init  .write(out)
      }

      def dispose()(implicit tx: S#Tx): Unit = {
        edges.dispose()
        tail .dispose()
        init .dispose()
      }

      override def equals(that: Any): Boolean = that match {
        case m: Mutable[_, _] =>
          id == m.id
        case _ => super.equals(that)
      }

      override def hashCode = id.hashCode()

      override def toString = s"Node$id"
    }

    case object RootNode extends RootOrNode {
      def edges = rootEdges
      override def toString = "0"

      def write(out: DataOutput): Unit = out.writeByte(0)
    }

    sealed trait Edge extends Writable {
      /*
       * The position in the corpus the edge's starting point corresponds to
       */
      def startIdx: Int
      /*
       * The position in the corpus the edge's stopping point corresponds to
       */
      def stopIdx: Int
      /*
       * Same as `stopIdx - startIdx`
       */
      def span: Int
      /*
       * The target node the edge is pointing to
       */
      def targetNode: NodeOrLeaf
      /*
       * Creates a copy of this edge with the starting index advanced. This is used in node splitting
       */
      def replaceStart(newStart: Int): Edge
    }

    /*
     * An edge going from a root or inner node to another inner node.
     */
    final case class InnerEdge(startIdx: Int, stopIdx: Int, targetNode: Node) extends Edge {
      override def toString = s"InnerEdge(start=$startIdx, stop=$stopIdx, target=$targetNode)"
      def span = stopIdx - startIdx
      def replaceStart(newStart: Int) = copy(startIdx = newStart)

      def write(out: DataOutput): Unit = {
        out.writeByte(0)
        out.writeInt(startIdx)
        out.writeInt(stopIdx )
        targetNode.write(out)
      }
    }

    /*
     * An edge going from a root or inner node to a leaf. Therefore the `stopIdx` corresponds to the
     * size of the corpus. If the corpus grows, `stopIdx` will reflect this accordingly.
     */
    final case class LeafEdge(startIdx: Int) extends Edge {
      override def toString = "LeafEdge(start=" + startIdx + ")"
      def targetNode: NodeOrLeaf = Leaf
      def stopIdx = corpus.length
      def span    = corpus.length - startIdx
      def replaceStart(newStart: Int) = copy(startIdx = newStart)

      def write(out: DataOutput): Unit = {
        out.writeByte(1)
        out.writeInt(startIdx)
      }
    }

    override def toString() = s"ContextTree(len=${corpus.length})@${hashCode().toHexString}"

    private def mkCursor()(implicit tx: S#Tx): Cursor = {
      val cID     = tx.newID()
      val cSource = tx.newVar(cID, RootNode: RootOrNode)
      val cStart  = tx.newIntVar(cID, 0)
      val cStop   = tx.newIntVar(cID, 0)
      val cExh    = tx.newBooleanVar(cID, init = false)
      new Cursor(id, source = cSource, startIdx = cStart, stopIdx = cStop, exhausted = cExh)
    }

    def snake(init: TraversableOnce[A])(implicit tx: S#Tx): Snake[S, A] = {
      val body    = init.toBuffer
      val c       = mkCursor()
      if (!init.forall(c.tryMove)) throw new NoSuchElementException(init.toString)

      new SnakeImpl(body, c)
    }

    def contains(elem: A)(implicit tx: S#Tx): Boolean = {
      val point = pointView(elem)
      RootNode.edges.isDefinedAt(point)
    }

    def containsSlice(xs: TraversableOnce[A])(implicit tx: S#Tx): Boolean = {
      val c   = mkCursor()
      val res = xs.forall(c.tryMove)
      c.dispose()
      res
    }

    def size: Int           = corpus.length
    def length: Int         = corpus.length
    def isEmpty: Boolean    = corpus.isEmpty
    def nonEmpty: Boolean   = corpus.nonEmpty
    def apply(idx: Int): A  = corpus(idx)

    def view(from: Int, until: Int): SeqView[A, mutable.Buffer[A]] = corpus.view(from, until)
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = corpus.to(cbf)

    def toDOT(tailEdges: Boolean, sep: String)(implicit tx: S#Tx): String = {
      val sb = new StringBuffer()
      sb.append("digraph suffixes {\n")

      var leafCnt = 0

      def appendNode(source: RootOrNode)(implicit tx: S#Tx): Unit = {
        sb.append("  " + source + " [shape=circle];\n")
        source.edges.iterator.foreach { case (_, edge) =>
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

    /*
     * Splits the edge according at an offset corresponding to the `active`'s span.
     * This produces a new inner node. The `edge`'s `source` node will be updated
     * to replace `edge` by a new edge which points to this new inner node. The old
     * `edge` itself will be truncated at the beginning, and replaced.
     *
     * The method returns the new node to which the `add1` algorithm can add
     * another outgoing leaf edge.
     */
    @inline private def split(edge: Edge)(implicit tx: S#Tx): Node = {
      val startIdx         = edge.startIdx
      val startElem        = corpus(startIdx)
      val splitIdx         = startIdx + active.span
      val newEdges         = SkipOctree.empty[S, D, (A, Edge)](hyperCube)
      val newNodeID        = tx.newID()
      val newParent        = active.source()
      val newNode          = new Node(newNodeID, newParent, newEdges, tx.newVar(newNodeID, newParent),
                                                                      tx.newVar(newNodeID, newParent))
      val newEdge1         = InnerEdge(startIdx, splitIdx, newNode)
      active.source().edges += ((startElem, newEdge1))
      val newEdge2         = edge.replaceStart(splitIdx)
      newNode.edges       += ((corpus(splitIdx), newEdge2))
      edge.targetNode match {
        case n: Node => n.init() = newNode
        case _ =>
      }
      DEBUG_LOG("SPLIT: " + edge + " -> new1 = " + newEdge1 + "; new2 = " + newEdge2)
      newNode
    }

    def +=(elem: A)(implicit tx: S#Tx): this.type = {
      add1(elem)
      this
    }

    def append   (elem: A*              )(implicit tx: S#Tx): Unit = elem foreach add1
    def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit = xs   foreach add1

    private def add1(elem: A)(implicit tx: S#Tx): Unit = {
      val elemIdx     = corpus.length
      corpus         += elem

      DEBUG_LOG(s"ADD: elem=$elem; $active")

      def addLink(n: RootOrNode, parent: RootOrNode): Unit =
        n match {
          case n: Node =>
            DEBUG_LOG(s"LINK: from $n to $parent")
            n.tail() = parent
          case RootNode =>
        }

      @tailrec def loop(prev: RootOrNode): RootOrNode = {
        val aSource = active.source()
        val parent = if (active.isExplicit) {
          // if we are on an explicit node which already has an outgoing edge for the element, we're done
          val point = pointView(elem)
          if (aSource.edges.isDefinedAt(point)) return prev
          // otherwise use this node as source for a new edge
          aSource
        } else {
          val activeElem  = corpus(active.startIdx())
          val activePoint = pointView(activeElem)
          val edge        = aSource.edges.get(activePoint)
            .getOrElse(throw new NoSuchElementException(activeElem.toString))._2
          // if we are on an implicit node and the next element equals the given element, we're done
          if (corpus(edge.startIdx + active.span) == elem) return prev
          // otherwise submit the edge representing the implicit node to a split, returning the
          // new source node to which the new (leaf) edge can be added
          split(edge)
        }

        // create new leaf edge starting at the parent node
        val newEdge = LeafEdge(elemIdx)
        parent.edges += ((elem, newEdge))
        addLink(prev, parent)

        // drop to tail suffix
        active.dropToTail()

        loop(parent)
      }

      val last = loop(RootNode)
      addLink(last, active.source())
      active.stopIdx.transform(_ + 1)
      active.canonize()
    }
  }
}

/** A mutable data append-only suffix tree that support efficient searching for sub-sequences.
  *
  * @tparam A  the element type of the structure
  */
trait ContextTree[S <: Sys[S], A] extends ContextTree.Like[S, A] with Mutable[S#ID, S#Tx] {
  /** Appends an element to the tree.
    *
    * @param elem  the element to append
    * @return      this same tree
    */
  def +=(elem: A)(implicit tx: S#Tx): this.type

  /** Appends multiple elements to the tree
    *
    * @param elems the elements to append
    */
  def append(elems: A*)(implicit tx: S#Tx): Unit

  /** Appends all elements of a collection to the tree. The elements are
    * appended in the order in which they are contained in the argument.
    *
    * @param xs  the collection whose elements should be appended
    */
  def appendAll(xs: TraversableOnce[A])(implicit tx: S#Tx): Unit

  /** Tests whether a given sub-sequence is contained in the tree.
    * This is a very fast operation taking O(|xs|).
    *
    * @param xs  the sequence to look for
    * @return    `true` if the sequence is included in the tree, `false` otherwise
    */
  def containsSlice(xs: TraversableOnce[A])(implicit tx: S#Tx): Boolean

  /** Tests whether an element is contained in the tree.
    * This is a constant time operation.
    *
    * @param elem  the element to look for
    * @return    `true` if the element is included in the tree, `false` otherwise
    */
  def contains(elem: A)(implicit tx: S#Tx): Boolean

  /** Creates a new snake through the tree from a given initial sequence.
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
  def snake(init: TraversableOnce[A])(implicit tx: S#Tx): ContextTree.Snake[S, A]

  /** Queries the number of elements in the tree. */
  def size: Int

  /** The length of the collection in this tree. Same as `size`. */
  def length: Int

  /** Queries whether the collection is empty (has zero elements). */
  def isEmpty: Boolean

  /** Queries whether the collection non-empty (has one or more elements). */
  def nonEmpty: Boolean

  /** Queries an element at a given index. Throws an exception if the `idx` argument
    * is negative or greater than or equal to the size of the tree.
    *
    * @param idx the index of the element
    * @return  the element at the given index
    */
  def apply(idx: Int): A

  /** Provides a view of a range of the underlying buffer. Technically, because
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

  /** Converts this tree into another collection by copying all elements.
    *
    * @param cbf   the builder factory which determines the target collection type
    * @tparam Col  the target collection type
    * @return  a new independent collection containing all elements of this tree
    */
  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A]

  /** Helper method to export the tree to GraphViz DOT format.
    * This is mostly for debugging or demonstration purposes and might not be
    * particularly efficient or suitable for large trees.
    *
    * @param tailEdges whether to include the tail (suffix-pointer) edges or not
    * @return  a string representation in DOT format
    */
  def toDOT(tailEdges: Boolean = false, sep: String = "")(implicit tx: S#Tx): String
}