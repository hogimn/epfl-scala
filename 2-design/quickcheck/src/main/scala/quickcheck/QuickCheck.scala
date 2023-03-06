package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =
    for
      a <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(a, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    val (small, large) = if a < b then (a, b) else (b, a)
    findMin(insert(large, insert(small, empty))) == small
  }

  property("gen3") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("gen4") = forAll { (h1 : H, h2: H) =>
    def sort(h : H) : List[Int] =
      if isEmpty(h) then List.empty
      else findMin(h) :: sort(deleteMin(h))

    sort(meld(h1, h2)) ==
      sort(meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("gen5") = forAll { (h1 : H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }