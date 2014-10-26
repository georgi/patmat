package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._

import patmat.Huffman._

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
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine more leaves") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('a', 8))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), Leaf('a', 8)))
  }

  test("combine less leaves") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)))
  }

  test("combine: insert merged leaves at correct position") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 2), Leaf('x', 3))
    assert(combine(leaflist) === List(Leaf('x', 3), Fork(Leaf('e', 2), Leaf('t', 2), List('e', 't'), 4)))
  }

  test("Until on 2 leaves") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(until(singleton, combine)(leaflist) === Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3))
  }

  test("Until on 3 leaves") {
    val leaflist = List(Leaf('a', 1), Leaf('b', 2), Leaf('c', 3))
    assert(until(singleton, combine)(leaflist) === Fork(Fork(Leaf('a', 1), Leaf('b', 2), List('a', 'b'), 3), Leaf('c', 3), List('a', 'b', 'c'), 6))
  }

  test("Create code tree") {
    val leaflist = List(Leaf('a', 1), Leaf('b', 2), Leaf('c', 3))
    assert(createCodeTree(List('a', 'b', 'b', 'c', 'c', 'c')) === Fork(Fork(Leaf('a', 1), Leaf('b', 2), List('a', 'b'), 3), Leaf('c', 3), List('a', 'b', 'c'), 6))
  }

  test("Simple decode") {
    val tree = Fork(Leaf('a'), Leaf('b'))

    assertEquals(List('a'), decode(tree, List(0)))
    assertEquals(List('b'), decode(tree, List(1)))
    assertEquals(List('a', 'a'), decode(tree, List(0, 0)))
  }

  test("test french") {
    print(decodedSecret.mkString)
  }

  test("Create code table") {
    val tree = Fork(Leaf('a'), Leaf('b'))
    assertEquals(List(('a', List(0)),('b', List(1))), convert(tree))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode") {
    new TestTrees {
      assertEquals(List(0, 1, 0, 1), encode(t2)("bb".toList))
    }
  }

  test("decode and encode a another short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("dabb".toList)) === "dabb".toList)
    }
  }
}
