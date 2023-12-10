import ChapterThree.MyList
import org.scalatest.flatspec.AnyFlatSpec

class ChapterThreeSpec extends AnyFlatSpec {
  "first exercise" should "return 3" in {
    assert(ChapterThree.MyList.exercise1 === 3)
  }

  "tail" should "return tail of a list" in {
    assert(MyList.tail(MyList(1, 2, 3)) === MyList(2, 3))
  }

  "setHead" should "set the head of a list" in {
    assert(MyList.setHead(MyList(1, 2, 3), 61) === MyList(61, 2, 3))
  }

  "dropN" should "drop an arbitrary number of elems from a list" in {
    assert(MyList.dropN(MyList(1, 2, 3), 61) === MyList())
    assert(MyList.dropN(MyList(1, 2, 3), 0) === MyList(1, 2, 3))
    assert(MyList.dropN(MyList(1, 2, 3), 1) === MyList(2, 3))
    assert(MyList.dropN(MyList(1, 2, 3), 2) === MyList(3))
  }

  "dropWhile" should "drop if f returns true given the elems in list" in {
    assert(MyList.dropWhile(MyList(1, 2, 3), _ > 0) === MyList())
    assert(MyList.dropWhile(MyList(1, 2, 3), _ > 1) === MyList(1))
    assert(MyList.dropWhile(MyList(1, 2, 3), _ > 61) === MyList(1, 2, 3))
  }

  "init" should "should dropLast of a list" in {
    assert(MyList.init(MyList(1, 2, 3)) === MyList(1, 2))
  }
}

