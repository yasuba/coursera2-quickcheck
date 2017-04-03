package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[A]
    } yield insert(x, empty)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h2 = insert(b, h)
    val min = List(a,b).min
    findMin(h2) == min
  }

  property("min3") = forAll { (a:Int, b: Int) =>
   val h = insert(a, empty)
    val h2 = insert(a,insert(b, h))
    findMin(meld(h, h2)) == List(a,b).min
  }

  property("deleteMin") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("melding") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h2 = insert(b, empty)
    findMin(meld(h,h2)) == List(a,b).min
  }

  property("min4") = forAll { (a: Int, b: Int, c:Int) =>
    val sortedList = List(a,b,c).sorted
    val h = insert(a, empty)
    val h2 = insert(b, h)
    findMin(insert(c, h2)) == sortedList.head
  }

  property("deleteMin2") = forAll {(a: Int, b: Int, c:Int) =>
    val reverseSortedList = List(a,b,c).sorted.reverse
    val h = insert(c,insert(b,insert(a, empty)))
    val h2 = deleteMin(h)
    deleteMin(h2) == insert(reverseSortedList.head, empty)
  }



}
