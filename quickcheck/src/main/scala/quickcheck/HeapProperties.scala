package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*
import scala.util.Random

class HeapProperties(heapInterface: HeapInterface) extends Properties("Heap"):

  // Import all the operations of the `HeapInterface` (e.g., `empty`
  // `insert`, etc.)
  import heapInterface.*

  // recursively traverse the heap
  private def check(heap: List[Node]): Boolean =
    // if the heap is empty, or if it has just one element, we have
    // successfully finished our checks
    if isEmpty(heap) || isEmpty(deleteMin(heap)) then true
    else
      // find the minimal element
      val x1: Int = findMin(heap)
      // delete the minimal element of `heap`
      val heap2: List[Node] = deleteMin(heap)
      // find the minimal element in `heap2`
      val x2: Int = findMin(heap2)
      // check that the deleted element is smaller than the minimal element
      // of the remaining heap, and that the remaining heap verifies the
      // same property (by recursively calling `check`)
      val checked: Boolean = x1 <= x2 && check(heap2)
      checked

  // Examples of properties
  property("inserting the minimal element and then finding it should return the same minimal element") = forAll { (heap: List[Node]) =>
    val min = if isEmpty(heap) then 0 else findMin(heap)
    findMin(insert(min, heap)) == min
  }

  property("the minimum of a heap of two elements should be the smallest of the two elements") = forAll { (x1: Int, x2: Int) =>
    val heap = insert(x2, insert(x1, empty))
    val min: Int = x1.min(x2)
    findMin(heap) == min
  }

  property("the minimum of a heap of two elements after the deletion of the minimum should be the biggest of the two elements") = forAll {
    (x1: Int, x2: Int) =>
      val heap = insert(x2, insert(x1, empty))
      val max: Int = x1.max(x2)
      findMin(deleteMin(heap)) == max
  }

  property("delete minumum of heap of one element should return an empty heap") = forAll { (x: Int) =>
    // create a heap with exactly one element, `x`
    val heap1: List[Node] = insert(x, empty)
    // delete the minimal element from it
    val heap0: List[Node] = deleteMin(heap1)
    // check that heap0 is empty
    heap0.isEmpty == true
  }

  property("continually finding and deleting the minimal element of a heap should return a sorted sequence") =
    // check arbitrary heaps
    forAll { (heap: List[Node]) =>
      check(heap)
    }

  // in bogus BinomialHeap implementations
  property(
    "inserting a list of integer into an empty heap and then continually finding and deleting the minimal element of a heap should return a sorted sequence"
  ) = forAll { (input: List[Int]) =>
    val heap = input.foldLeft(empty)((h, i) => insert(i, h))
    check(heap)
  }

  property("the minimum should be equal or higher in the heap after deleting minimum is perfomed") = forAll(genNonEmptyHeap) {
    (heap: List[Node]) =>
      val min = findMin(heap)
      val heap2 = deleteMin(heap)
      val min2 = if heap2.isEmpty then min else findMin(heap2)
      min <= min2
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") =
    // check arbitrary heaps
    forAll(genNonEmptyHeap, genNonEmptyHeap) { (heap1: List[Node], heap2: List[Node]) =>
      val meldingHeap = meld(heap1, heap2)
      val minHeap1 = findMin(heap1)
      val minHeap2 = findMin(heap2)
      val minMeldingHeap = findMin(meldingHeap)
      minMeldingHeap == minHeap1 || minMeldingHeap == minHeap2
    }

  property("Melding two heaps should result in a heap with the same amount of elements") = forAll {
    (heap1: List[Node], heap2: List[Node]) =>
      def flattenHeap(h: List[Node]): List[Node] = h.flatMap(x => x :: flattenHeap(x.children))
      val meldingHeap = meld(heap1, heap2)
      flattenHeap(meldingHeap).length == (flattenHeap(heap1).length + flattenHeap(heap2).length)
  }

  property("Melding two heaps should return a sorted sequence") = forAll { (heap1: List[Node], heap2: List[Node]) =>
    check(meld(heap1, heap2))
  }

  property("shuffling a list of nodes should affect the delete min") = forAll(Gen.nonEmptyListOf(Gen.choose(0, Int.MaxValue))) {
    (input: List[Int]) =>
      val input2 = input.distinct
      val heap = input2.foldLeft(empty)((h, i) => insert(i, h))
      val heap2 = Random.shuffle(heap)
      val heap3 = deleteMin(heap2)
      //println(s"heap2: $heap2 - min $min - heap3: $heap3")
      if heap3.isEmpty then true else input2.min < findMin(heap3)
  }

  // random heap generator --- DO NOT MODIFY
  private lazy val genHeap: Gen[List[Node]] = oneOf(
    const(empty),
    genNonEmptyHeap
  )

  // random non empty heap generator --- DO NOT MODIFY
  private lazy val genNonEmptyHeap: Gen[List[Node]] =
    for
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(v, h)

  private given Arbitrary[List[Node]] = Arbitrary(genHeap)

end HeapProperties
