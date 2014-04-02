package de.sciss.contextsnake.txn

import org.scalatest.{Outcome, Matchers, FunSuite, fixture}
import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.contextsnake.txn.impl.LinkedList

/*
  to run only this test:

  test-only de.sciss.contextsnake.txn.LinkedListSuite
 */
class LinkedListSuite extends fixture.FunSuite with Matchers {
  final type S = Durable
  final type FixtureParam = Durable

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  test("testing should be easy") { cursor =>
    val liH = cursor.step { implicit tx =>
      val li = LinkedList(1, 2, 3)
      assert(li.nonEmpty)
      assert(!li.isEmpty)
      assert(li.size == 3)
      assert(li.headOption == Some(1))
      assert(li.iterator.toList == List(1, 2, 3))
      assert(li(0) == 1)
      assert(li(1) == 2)
      assert(li(2) == 3)

      li.trimStart(0)
      assert(li.size === 3)
      li.trimEnd(0)
      assert(li.size === 3)

      tx.newHandle(li)
    }

    cursor.step { implicit tx =>
      assert(liH().iterator.toList == List(1, 2, 3))
    }
  }
}