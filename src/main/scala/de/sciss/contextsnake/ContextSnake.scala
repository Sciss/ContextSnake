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
  def apply[A](elem: A*): ContextSnake[A] = {
    val res = new Impl[A]
    elem.foreach(res.append)
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

    type EdgeKey  = (Int, A)

    private final class Edge(var startIdx: Int, stopIdx: Int) {
      val targetNode = nodeCount
      nodeCount += 1

      def span = (if (stopIdx < 0) corpus.length else stopIdx) - startIdx

      override def toString = "Edge(start=" + startIdx + ", stop=" + stopIdx + ", target=" + targetNode + ")"
    }

    def contains(seq: Traversable[A]): Boolean = {
      ???
    }

    def toDOT(tailEdges: Boolean): String = {
      val elemSet = corpus.toSet
      val sb      = new StringBuffer()
      sb.append("graph suffixes {\n")

      def appendNode(source: Int) {
//        sb.append("  " + source + " [shape=point, label=\"\", xlabel=\"" + source + "\"];\n")
        sb.append("  " + source + " [shape=circle];\n")
        val out = elemSet.flatMap { e => edges.get((source, e))}
        out.foreach { edge =>
          val str     = corpus.slice(edge.startIdx, edge.startIdx + edge.span).mkString("")
          val target  = edge.targetNode
          sb.append("  " + source + " -- " + target + " [label=\"" + str + "\"];\n")
          appendNode(target)
        }
      }
      appendNode(0)

      sb.append("}\n")
      sb.toString
    }

    @inline private def isExplicit = activeStartIdx >= activeStopIdx

    @inline private def split(edge: Edge): Int = {
      val startIdx    = edge.startIdx
      val startElem   = corpus(startIdx)
      val activeSpan  = activeStopIdx - activeStartIdx
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
        val activeSpan  = activeStopIdx - activeStartIdx
        if (edgeSpan > activeSpan) return
        activeStartIdx += edgeSpan
        activeNode      = edge.targetNode
      }
    }

    def append(elem: A) {
      val oldLen      = corpus.length
      corpus         += elem
      var parent      = -1
      var prevParent  = -1

      def finish() {
        // TODO: DRY - this is done in the while loop as well
        if (prevParent > 0) {
          tails(prevParent) = parent
        }
        activeStopIdx += 1
        canonize()
      }

      while (true) {
        parent = if (isExplicit) {
          if (edges.contains((activeNode, elem))) {
            finish()
            return
          }
          activeNode
        } else {
          val edge = edges((activeNode, corpus(activeStartIdx)))
          if (corpus(edge.startIdx + (activeStopIdx - activeStartIdx)) == elem) {
            finish()
            return
          }
          split(edge)
        }
        // create new leaf edge starting at parentNode
        val newEdge = new Edge(oldLen, -1 /*, parent */)
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
trait ContextSnake[A] {
  def append(elem: A): Unit
  def contains(seq: Traversable[A]): Boolean

  def toDOT(tailEdges: Boolean = false): String
}