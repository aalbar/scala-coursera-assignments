package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("test times") {
    assert(times(List('t', 'x', 'x', 'e', 't', 'x')) === List(('t', 2), ('x', 3), ('e', 1)))
  }
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("makeOrderedLeafList for more  frequency table") {
    assert(makeOrderedLeafList(List(('y', 4), ('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3), Leaf('y', 4)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of some leaf list with different weight") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('z', 6))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), Leaf('z', 6)))
  }

  test("until  some leaf list become single fork") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('z', 6))
    assert(until(singleton, combine)(leaflist) === List(Fork(Leaf('z', 6), Fork(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), List('x', 'e', 't'), 9), List('z', 'x', 'e', 't'), 15))
    )
  }


  test("create code tree given list of char") {
    val chars = List('a', 'b', 'd')
    assert(createCodeTree(chars) === Fork(Leaf('d', 1), Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2), List('d', 'a', 'b'), 3))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ababab".toList)) === "ababab".toList)
    }
  }

  test("decode and encode a  short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("badadadb".toList)) === "badadadb".toList)
    }
  }

}
