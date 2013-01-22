/*
 *  ContextSnake.scala
 *  (ContextSnake)
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

import collection.mutable

object ContextSnake {
  def empty[A]: ContextSnake[A] = new Impl[A]

  def apply[A](elem: A*): ContextSnake[A] = {
    val res = empty[A]
    res.appendAll(elem)
    res
  }

  private final class Impl[A] extends ContextSnake[A] {
    private val corpus            = mutable.Buffer.empty[A]
    private val tails             = mutable.Map.empty[Int, Int] withDefaultValue -1
    private val edges             = mutable.Map.empty[EdgeKey, Edge]
    private var activeNode        = 0
    private var activeStartIdx    = 0
    private var activeStopIdx     = 0
    private var nodeCount         = 1

    private type EdgeKey  = (Int, A)  // source-node-id -> first-element-on-edge-label

    private final class Edge(var startIdx: Int, stopIdxOption: Int) {
      val targetNode = nodeCount
      nodeCount += 1

      def stopIdx = if (stopIdxOption < 0) corpus.length else stopIdxOption
      def span    = stopIdx - startIdx

      override def toString = "Edge(start=" + startIdx + ", stop=" + stopIdxOption + ", target=" + targetNode + ")"
    }

    override def toString = "ContextSnake(len=" + corpus.length + ")@" + hashCode().toHexString

    def contains(xs: TraversableOnce[A]): Boolean = {
      var n     = 0
      var start = 0
      var stop  = 0
      xs.foreach { e =>
        if (start < stop) {
          if (corpus(start) != e) return false  // not found in implicit node
          start += 1
        } else edges.get((n, e)) match {
          case None       => return false       // reached end of leaf node
          case Some(edge) =>
            n     = edge.targetNode
            start = edge.startIdx + 1
            stop  = edge.stopIdx
        }
      }
      true
    }

    def size: Int = corpus.size
    def apply(idx: Int): A = corpus(idx)

    def toDOT(tailEdges: Boolean, sep: String): String = {
      val elemSet = corpus.toSet
      val sb      = new StringBuffer()
      sb.append("digraph suffixes {\n")

      def appendNode(source: Int) {
        sb.append("  " + source + " [shape=circle];\n")
        val out = elemSet.flatMap { e => edges.get((source, e))}
        out.foreach { edge =>
          val str     = corpus.slice(edge.startIdx, edge.stopIdx).mkString(sep)
          val target  = edge.targetNode
          sb.append("  " + source + " -> " + target + " [label=\"" + str + "\"];\n")
          appendNode(target)
        }
      }
      appendNode(0)

      if (tailEdges && tails.nonEmpty) {
        sb.append( "\n" )
        tails.foreach { case (source, target) =>
          sb.append("  " + source + " -> " + target + " [style=dotted];\n")
        }
      }

      sb.append("}\n")
      sb.toString
    }

    @inline private def isExplicit  = activeStartIdx >= activeStopIdx
    @inline private def activeSpan  = activeStopIdx - activeStartIdx

    @inline private def split(edge: Edge): Int = {
      val startIdx    = edge.startIdx
      val startElem   = corpus(startIdx)
      val splitIdx    = startIdx + activeSpan
      val newEdge     = new Edge(startIdx, splitIdx )
      edges          += (((activeNode, startElem), newEdge))
      val newNode     = newEdge.targetNode
      tails(newNode)  = activeNode
      edge.startIdx   = splitIdx
      edges          += (((newNode, corpus(splitIdx)), edge))
      newNode
    }

    @inline private def canonize() {
      while (!isExplicit) {
        val edge        = edges((activeNode, corpus(activeStartIdx)))
        val edgeSpan    = edge.span
        if (edgeSpan > activeSpan) return
        activeStartIdx += edgeSpan
        activeNode      = edge.targetNode
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
      var prevParent  = -1

      while (true) {
        val parent = if (isExplicit) {
          if (edges.contains((activeNode, elem))) {
            activeStopIdx += 1
            canonize()
            return
          }
          activeNode
        } else {
          val edge = edges((activeNode, corpus(activeStartIdx)))
          if (corpus(edge.startIdx + (activeStopIdx - activeStartIdx)) == elem) {
            activeStopIdx += 1
            canonize()
            return
          }
          split(edge)
        }
        // create new leaf edge starting at parentNode
        val newEdge = new Edge(elemIdx, -1 /*, parent */)
        edges += (((parent, elem), newEdge))
        if (prevParent > 0) {
          tails(prevParent) = parent
        }
        prevParent = parent

        // drop to tail suffix
        if (activeNode == 0) {
          activeStartIdx += 1
        } else {
          activeNode = tails(activeNode)
          canonize()
        }
      }
    }
  }
}

/**
 * A mutable data append-only structure that support efficient searching for sub-sequences.
 * In this version, it is just a suffix tree.
 *
 * @tparam A  the element type of the structure
 */
trait ContextSnake[A] {
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
   *
   * @param xs  the sequence to look for
   * @return    `true` if the sequence is included in the tree, `false` otherwise
   */
  def contains(xs: TraversableOnce[A]): Boolean

  /**
   * Queries the number of elements in the tree
   */
  def size: Int

  /**
   * Queries an element at a given index. Throws an exception if the `idx` argument
   * is negative or greater than or equal to the size of the tree.
   *
   * @param idx the index of the element
   * @return  the element at the given index
   */
  def apply(idx: Int): A

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