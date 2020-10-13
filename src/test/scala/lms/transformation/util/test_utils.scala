package lms
package transformation.util

class USetSpec extends TutorialFunSuite with DataStructure {

  val under = ""

  test("USetIsWorking1") {
    val usets = new USets[Int](List(1,2,3,4))
    usets.merge(1, 3)
    assert(usets.union_map(1) == 1)
    assert(usets.union_map(2) == 2)
    assert(usets.union_map(3) == 1)
    assert(usets.union_map(4) == 4)
  }

  test("USetIsWorking2") {
    val usets = new USets[Int](List(1,2,3,4))
    usets.merge(1, 3)
    usets.merge(2, 4)
    assert(usets.union_map(1) == 1)
    assert(usets.union_map(2) == 2)
    assert(usets.union_map(3) == 1)
    assert(usets.union_map(4) == 2)
  }

  test("USetIsWorking3") {
    val usets = new USets[Int](List(1,2,3,4))
    usets.merge(1, 3)
    usets.merge(2, 4)
    usets.merge(3, 4)
    assert(usets.union_map(1) == 1)
    assert(usets.union_map(2) == 1)
    assert(usets.union_map(3) == 1)
    assert(usets.union_map(4) == 1)
  }

}
