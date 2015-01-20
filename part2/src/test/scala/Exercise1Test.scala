package exercise1

import org.scalatest.{FunSpec, FunSuite}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Exercise1Test extends FunSpec {
  describe("Треугольник паскаля") {
    import Exercise1.pascal
    it("pascal: col=0,row=2") {
      assert(pascal(0, 2) === 1)
    }

    it("pascal: col=1,row=2") {
      assert(pascal(1, 2) === 2)
    }

    it("pascal: col=1,row=3") {
      assert(pascal(1, 3) === 3)
    }
  }

  describe("Балансировка скобок") {
    import Exercise1.balance

    it("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
      assert(balance("(if (zero? x) max (/ 1 x))".toList))
    }

    it("balance: 'I told him ...' is balanced") {
      assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    }

    it("balance: ':-)' is unbalanced") {
      assert(!balance(":-)".toList))
    }

    it("balance: counting is not enough") {
      assert(!balance("())(".toList))
    }
  }

  describe("Подсчет вариантов размена суммы") {
    import Exercise1.countChange
    it("countChange: example given in instructions") {
      assert(countChange(4,List(1,2)) === 3)
    }

    it("countChange: sorted CHF") {
      assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
    }

    it("countChange: no pennies") {
      assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
    }

    it("countChange: unsorted CHF") {
      assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
    }
  }
}