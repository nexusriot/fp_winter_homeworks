package huffman

import huffman.Huffman._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
    assert(makeOrderedLeafList(List( ('s', 1), ('o', 1), ('m', 1), ('e', 2), ('t', 2), ('x', 1) )) === List(Leaf('s', 1), Leaf('o', 1), Leaf('m',1), Leaf('x', 1), Leaf('e', 2), Leaf('t', 2)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("encode should be ok") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0, 1))
      assert(encode(t2)("abd".toList) === List(0,0,0,1,1))
    }
  }

  test("createCodeTree should be optimal") {
     // Fork(Fork(Fork(Fork(Fork(Leaf('s', 1), Leaf('o', 1), "so".toList, 2), Leaf('m',1), "som".toList, 3), Leaf('x', 1), "somx".toList, 4), Leaf('e', 2), "somxe".toList, 6), Leaf('t', 2), "somxet".toList, 8))
   //assert(createCodeTree("sometext".toList) === Fork(Fork(Fork(Fork(Fork(Leaf('x',1),Leaf('m',1),"xm".toList,2),Leaf('o',1),"xmo".toList,3),Leaf('s',1),"xmos".toList,4),Leaf('t',2),"xmost".toList,6),Leaf('e',2),"xmoste".toList,8))
   //assert(createCodeTree("AIR STRIKES ALONE".toList) === Fork(Fork(Fork(Fork(Fork(Leaf('x',1),Leaf('m',1),"xm".toList,2),Leaf('o',1),"xmo".toList,3),Leaf('s',1),"xmos".toList,4),Leaf('t',2),"xmost".toList,6),Leaf('e',2),"xmoste".toList,8))
  }

  test("encode text should be optimal") {
   assert(encode(createCodeTree("sometext".toList))("sometext".toList).size === 20)
   assert(encode(createCodeTree("AIR STRIKES ALONE".toList))("AIR STRIKES ALONE".toList).size === 58)
   assert(encode(createCodeTree("IN YOU IVE FOUND".toList))("IN YOU IVE FOUND".toList).size === 52)
  }

  test("decode should be ok") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === List('a','b'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quick encode a very short text should be ok") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0, 1))
    }
  }
}
