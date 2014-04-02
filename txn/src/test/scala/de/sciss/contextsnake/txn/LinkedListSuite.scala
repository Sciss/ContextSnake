package de.sciss.contextsnake.txn

import org.scalatest.{Outcome, Matchers, FunSuite, fixture}
import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.contextsnake.txn.impl.LinkedList

/*
  to run only this test:

  contextsnake-txn/test-only de.sciss.contextsnake.txn.LinkedListSuite

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
      assert(li.size === 3)
      assert(li.headOption === Some(1))
      assert(li.iterator.toList === List(1, 2, 3))
      assert(li(0) === 1)
      assert(li(1) === 2)
      assert(li(2) === 3)

      li.trimStart(0)
      assert(li.size === 3)
      li.trimEnd(0)
      assert(li.size === 3)

      intercept[IndexOutOfBoundsException](li(-1))
      intercept[IndexOutOfBoundsException](li(4))
      intercept[IndexOutOfBoundsException](li.trimStart(4))
      intercept[IndexOutOfBoundsException](li.trimEnd  (4))

      assert(!li.remove(0))
      assert(!li.remove(4))

      tx.newHandle(li)
    }

    cursor.step { implicit tx =>
      val li = liH()
      assert(li.iterator.toList === List(1, 2, 3))

      assert(li.remove(1))
      assert(li.size === 2)
      assert(li.iterator.toList === List(2, 3))

      li.trimStart(2)
      assert(li.isEmpty)
      assert(!li.nonEmpty)
      assert(li.size == 0)
      assert(li.headOption === None)
      assert(li.iterator.toList === Nil)

      li.append(4)
      li.prepend(5)
      li.append(6)

      assert(li.size === 3)
      assert(li.headOption === Some(5))
      li.trimEnd(3)
      assert(li.isEmpty)

      li.appendAll(List(1, 2, 3))
      li.appendAll(List(4, 5, 6))
      assert(li.iterator.toList === List(1, 2, 3, 4, 5, 6))
    }
  }
}