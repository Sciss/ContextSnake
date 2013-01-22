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
//    private val nodes             = mutable.Buffer.empty[Node]
    private val tails             = mutable.Map.empty[Int, Int] withDefaultValue -1
    private val edges             = mutable.Map.empty[EdgeKey, Edge]
    private var activeNode        = 0 // node
    private var activeStartIdx    = 0 // start
    private var activeStopIdx     = 0 // stop
    private var nodeCount         = 1

    type EdgeKey  = (Int, A)
//    type Edge     = Int


    def contains(seq: Traversable[A]): Boolean = {
      ???
    }

    private final class Edge(var startIdx: Int, stopIdx: Int, var sourceNode: Int) {      // TODO: sourceNode not needed
      val targetNode = nodeCount
      nodeCount += 1

      def span = (if (stopIdx < 0) corpus.length else stopIdx) - startIdx

      override def toString = "Edge(start=" + startIdx + ", stop=" + stopIdx + ", source=" + sourceNode + ", target=" + targetNode + ")"
    }

//    private final class Node {
//      var suffixNode = -1
//      override def toString = "Node(suffix=" + suffixNode + ")@" + hashCode().toHexString
//    }

//    @inline private def isExplicit = activeStartIdx == activeStopIdx
    @inline private def isExplicit = activeStartIdx >= activeStopIdx

    @inline private def split(edge: Edge): Int = {
      val startIdx  = edge.startIdx
      val startElem = corpus(startIdx)
      edges -= ((edge.sourceNode, startElem)) // TODO: not necessary, because edge.sourceNode == activeNode (the entry will be overwritten further down)
      val activeSpan = activeStopIdx - activeStartIdx
      val newEdge = new Edge(startIdx, startIdx + activeSpan, activeNode)
      edges += (((activeNode, startElem), newEdge))
//      val newNode = new Node
//      newNode.suffixNode = activeNode
      val newNode = newEdge.targetNode // nodes.length
     tails(newNode) = activeNode
//      nodes += newNode
      val newStartIdx  = startIdx + activeSpan  // TODO: DRY (newEdge.stopIdx)
      edge.startIdx    = newStartIdx
      edge.sourceNode  = newNode
      edges += (((edge.sourceNode, corpus(newStartIdx)), edge))
      newNode
    }

    @inline private def canonize() { // TODO: "Finally, these modifications reduce the state canonization logic to simply "consume edges until the next edge is not small enough to consume or the state is explicit"."
      if (isExplicit) return

      var edge      = edges((activeNode, corpus(activeStartIdx)))
      var edgeSpan  = edge.span // stopIdx - edge.startIdx
      while (edgeSpan <= activeStopIdx - activeStartIdx ) {
        activeStartIdx += edgeSpan
        activeNode      = edge.targetNode
        if (activeStartIdx < activeStopIdx) {
          edge      = edges((activeNode, corpus(activeStartIdx)))
          edgeSpan  = edge.span // stopIdx - edge.startIdx
        }
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
//          nodes(prevParent).suffixNode = parent
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
        val newEdge = new Edge(oldLen, -1, parent)
        edges += (((parent, elem), newEdge))
        if (prevParent > 0) {
//          nodes(prevParent).suffixNode = parent
          tails(prevParent) = parent
        }
        prevParent = parent

        // drop to tail suffix
        if (activeNode == 0) {
          activeStartIdx += 1
        } else {
//          activeNode = nodes(activeNode).suffixNode
          activeNode = tails(activeNode)
          canonize()
        }
      }
    }
  }

//  private final class Suffix /* (node: Int, start: Int, stop: Int) */ {
//
//    def isExplicit  = startIdx == stopIdx
//    def isImplicit  = startIdx < stopIdx
//
//    def hasNext(elem: A): Boolean = {
//
//    }
//  }
}
trait ContextSnake[A] {
  def append(elem: A): Unit
  def contains(seq: Traversable[A]): Boolean
}