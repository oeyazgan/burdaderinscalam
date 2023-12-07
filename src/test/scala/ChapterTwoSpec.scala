import org.scalatest.flatspec.AnyFlatSpec

class ChapterTwoSpec extends AnyFlatSpec {
  "An empty List" should "have size 0" in {
    assert(List.empty.size === 0)
  }

  "isSorted" should "return true when array is sorted" in {
    assert(ChapterTwo.isSorted(Array(1, 2, 3, 4, 5), (a, b) => a > b))
  }

  it should "work for other types" in {
    assert(ChapterTwo.isSorted(Array("a", "b", "c"), (a, b) => a > b))
  }

  it should "return false when array is not sorted" in {
    assert(!ChapterTwo.isSorted(Array(1, 2, 3, 4, 3), (a, b) => a > b))
  }

  it should "return true when array is empty" in {
    assert(ChapterTwo.isSorted(Array.emptyByteArray, (a: Byte, b: Byte) => a > b))
  }

  "findFirstPolymorphic" should "return the index of first elem in array" in {
    assert(ChapterTwo.findFirstPolymorphic(Array("a", "s", "d", "a"), "d") === 2)
  }

  it should "work for different types" in {
    assert(ChapterTwo.findFirstPolymorphic(Array(1, 2, -1, 4), 2) === 1)
  }

  it should "return -1 when key is missing" in {
    assert(ChapterTwo.findFirstPolymorphic(Array(1, 2, -1, 4), 22) === -1)
  }

  it should "work for empty array" in {
    assert(ChapterTwo.findFirstPolymorphic(Array.emptyIntArray, 22) === -1)
  }
}
