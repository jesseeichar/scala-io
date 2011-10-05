package scalaio.test
import scalax.io.LongTraversable
import org.junit.Test
import org.junit.Assert._

trait DataIndependentLongTraversableTest[T] {
  def independentTraversable(): LongTraversable[T]
  def sample: Array[T]
  def times(t1: T, t2: T): T
  def lessThan(t: T, i: Int): Boolean
  def scanSeed: T
  def independentExpectedData: Seq[T]

  def assertSizeAndType[A](traversable: Traversable[T], f: Traversable[T] => Traversable[A], isLongTraversable: Boolean = true) = {
    val list = f(independentExpectedData.toList)
    val applied = f(traversable)
    if (isLongTraversable)
      assert(applied.isInstanceOf[LongTraversable[_]], "new traversable is not a LongTraversable: " + applied.getClass.getName)
    assertEquals(list.size, applied.size)

    assertEquals(list.toList, applied.toList)
  }

  def assertProductSizeAndType(traversable: Traversable[T], f: Traversable[T] => Product, areLongTraversable: Boolean = true) = {
    val list = f(independentExpectedData toList)
    val applied = f(traversable)
    if (areLongTraversable) {
      applied.productIterator foreach { t => assert(t.isInstanceOf[LongTraversable[_]], "new traversable is not a LongTraversable: " + applied.getClass.getName) }
    }
    assertEquals(list.productArity, applied.productArity)

    list.productIterator.zipWithIndex zip applied.productIterator foreach {
      case ((expected: Traversable[_], index), actual: Traversable[_]) =>
        assertEquals(expected.size, actual.size)
        assertEquals(expected.toList, actual.toList)
    }
  }

  @Test //@Ignore
  def should_handle_append = assertSizeAndType(independentTraversable(), _ ++ sample)

  @Test //@Ignore
  def size_should_work_like_lists = {
    val traversable2 = independentTraversable()

    assertSizeAndType(traversable2, t => t.slice(-1, 300))
    assertSizeAndType(traversable2, t => t.slice(0, 0))
    assertSizeAndType(traversable2, t => t.drop(300))
    assertSizeAndType(traversable2, t => t.drop(-1))
    assertSizeAndType(traversable2, t => t)
    assertSizeAndType(traversable2, t => t.drop(0))
    assertSizeAndType(traversable2, t => t.drop(0))
    assertSizeAndType(traversable2, t => t.drop(2))
    assertSizeAndType(traversable2, t => t.slice(2, 10))
    assertSizeAndType(traversable2, t => t.take(10).drop(5))
    assertSizeAndType(traversable2, t => t.take(10).drop(5).take(2))
    assertSizeAndType(traversable2, t => t.drop(10).take(5).drop(20))
    assertSizeAndType(traversable2, t => t.drop(10).drop(5).drop(20))
    assertSizeAndType(traversable2, t => t.slice(2, 10).slice(1, 5))
    assertSizeAndType(traversable2, t => t.slice(2, 10).drop(5))
  }
  @Test //@Ignore
  def should_handle_++ = assertSizeAndType(independentTraversable(), _ ++ List(1, 2))

  @Test //@Ignore
  def should_handle_drop = assertSizeAndType(independentTraversable(), _ drop 2)

  @Test //@Ignore
  def should_handle_drop_tomany = assertSizeAndType(independentTraversable(), _ drop 10000)

  @Test //@Ignore
  def should_handle_map = assertSizeAndType(independentTraversable(), _ map { i => times(i, i).toString })
  @Test //@Ignore
  def should_handle_map_slice = assertSizeAndType(independentTraversable(), t => t.map { i => times(i, i) }.slice(2, 10))

  @Test //@Ignore
  def should_handle_several_ops = assertSizeAndType(independentTraversable(), t => t ++ List(1, 2) map { _.toString })

  @Test //@Ignore
  def should_handle_scanLeft_Right = {
    val r = independentTraversable()
    assertSizeAndType(r, _.scanLeft(scanSeed) { times })
    assertSizeAndType(r, _.scanRight(scanSeed) { times })
  }

  @Test //@Ignore
  def should_handle_collect = {
    val r = independentTraversable()
    assertSizeAndType(r, _ collect { case i if lessThan(i, 3) => times(i, i) })
    assertSizeAndType(r, _ collect { case i if lessThan(i, 3) => "a" + i })
  }

  @Test //@Ignore
  def should_handle_groupBy = {
    def f(t: Traversable[T]) = t groupBy { _.toString()(0) }
    val list = f(independentExpectedData toList)
    val applied = f(independentTraversable()) map { case (k, v) => (k, v.toList) }

    assertEquals(list.size, applied.size)
    assertEquals(list.toList, applied.toList)
  }

  @Test //@Ignore
  def should_handle_init = 
    assertSizeAndType(independentTraversable(), _ init, false)

  @Test //@Ignore
  def should_handle_slice = assertSizeAndType(independentTraversable(), _ slice (3, 10))

  //@Test //@Ignore
  def should_handle_zip = {
    val actual = independentTraversable() zip (101 to 200)
    val expected = independentExpectedData zip (101 to 200)

    assertEquals(expected.toList, actual.toList)
  }

  @Test //@Ignore
  def should_handle_tail = 
    assertSizeAndType(independentTraversable(), _ tail)

  @Test //@Ignore
  def should_handle_take = {
    assertSizeAndType(independentTraversable(), _ take (10))
    assertSizeAndType(independentTraversable(), _ take (-1))
    assertSizeAndType(independentTraversable(), _ take (30000))
  }

  @Test //@Ignore
  def should_handle_unzip = assertProductSizeAndType(independentTraversable(), _.unzip(i => (1, i)))

  @Test //@Ignore
  def flatmap_then_slice = assertSizeAndType(independentTraversable(), t => t.flatMap { "x" + _ }.slice(2, 10))

  @Test //@Ignore
  def map_then_slice = assertSizeAndType(independentTraversable(), _ map { i => times(i, i).toString } slice (2, 10))

  @Test //@Ignore
  def map_then_take = assertSizeAndType(independentTraversable(), _ map { i => times(i, i).toString } take 10)

  @Test //@Ignore
  def drop_then_slice = assertSizeAndType(independentTraversable(), _ map { i => times(i, i).toString } drop 3 slice (2, 10))
  @Test //@Ignore
  def should_handle_splitAt = {
    assertProductSizeAndType(independentTraversable(), _ splitAt 13, false)
    assertProductSizeAndType(independentTraversable(), _ splitAt -1, false)
    assertProductSizeAndType(independentTraversable(), _ splitAt 50000, false)
  }

  @Test //@Ignore
  def should_handle_dropWhile = assertSizeAndType(independentTraversable(), _ dropWhile { lessThan(_, 55) })

  @Test //@Ignore
  def should_handle_filter = assertSizeAndType(independentTraversable(), _ filter { lessThan(_, 55) })

  @Test //@Ignore
  def should_handle_filterNot = assertSizeAndType(independentTraversable(), _ filterNot { lessThan(_, 55) })

  @Test //@Ignore
  def should_handle_takeWhile = assertSizeAndType(independentTraversable(), _ takeWhile { lessThan(_, 23) })

  @Test //@Ignore
  def should_handle_partition = {
    assertProductSizeAndType(independentTraversable(), _ span { lessThan(_, 30) })
    assertProductSizeAndType(independentTraversable(), _ span { lessThan(_, -1) })
  }

  @Test //@Ignore
  def filter_then_slice = assertSizeAndType(independentTraversable(), _ filter { lessThan(_, 45) } slice (2, 10))

  @Test //@Ignore
  def filter_then_take = assertSizeAndType(independentTraversable(), _ filter { lessThan(_, 45) } take 10)

  @Test //@Ignore
  def filter_then_drop = assertSizeAndType(independentTraversable(), _ filter { lessThan(_, 45) } drop 10)

  @Test //@Ignore
  def drop_then_take = assertSizeAndType(independentTraversable(), _ drop 10 take 10)

  @Test //@Ignore
  def drop_then_drop = assertSizeAndType(independentTraversable(), _ drop 45 drop 10)

  @Test //@Ignore
  def drop_then_append = assertSizeAndType(independentTraversable(), t => (t drop 45) ++ List(1, 2, 3))

  @Test //@Ignore
  def drop_to_many = assertSizeAndType(independentTraversable(), t => (t drop 1000))

  @Test //@Ignore
  def slice_to_0 = assertSizeAndType(independentTraversable(), t => (t slice (5, 10) slice (0, -3)))

  @Test //@Ignore
  def slice_then_unzip = assertProductSizeAndType(independentTraversable(), _ slice (3, 10) unzip { i => (1, i) })

  @Test //@Ignore
  def append_then_drop =
    assertSizeAndType(independentTraversable(), t => t ++ sample drop 98)

  @Test //@Ignore
  def size = assertFalse(independentTraversable().hasDefiniteSize)

  def traversable_should_be_LongTraversable = {
    independentTraversable().ltake(3L) // if this compiles it passes the test
  }

  @Test
  def apply {
    val input = independentTraversable()
    val expected = independentExpectedData
    0 until expected.size foreach { i =>
      assertEquals(expected(i), input(i))
    }
  }

  @Test
  def ldrop {
    val input = independentTraversable()
    val expected = independentExpectedData
    assertEquals(expected.drop(10).size, input.ldrop(10).size)
  }
  @Test
  def ltake {
    val input = independentTraversable()
    val expected = independentExpectedData
    assertEquals((expected take 10).size, input.ltake(10).size)
  }
  @Test
  def lslice {
    val input = independentTraversable()
    val expected = independentExpectedData
    assertEquals(expected.slice(2, 10).size, input.lslice(2, 10).size)
    assertEquals(expected.slice(11, 10).size, input.lslice(11, 10).size)
    assertEquals(expected.take(10).size, input.lslice(-1, 10).size)
    assertEquals(expected.take(10).size, input.lslice(-1, 10).lslice(-1, 10).size)
    val sliced = input.lslice(2, Long.MaxValue)
    assertEquals(expected.slice(2, Int.MaxValue).size, sliced.size)
  }
  @Test
  def isDefinedAt {
    val input = independentTraversable()
    assertTrue(input.isDefinedAt(5))
    assertFalse(input.isDefinedAt(-1))
    assertFalse(input.isDefinedAt(300))
  }

  @Test
  def splitAt = {
    val input = independentTraversable()
    val expected = independentExpectedData

    val splitPoint1 = expected.size / 2
    val (a1, a2) = input.splitAt(splitPoint1)
    val (e1, e2) = expected.splitAt(splitPoint1)
    assertEquals(e1.toList, a1.toList)
    assertEquals(e2.toList, a2.toList)
    
    
  }
  @Test
  def indexOf {
    val input = independentTraversable()
    val expected = independentExpectedData
    assertEquals(expected.indexOf(200), input.indexOf(200))
    assertEquals(expected.indexOf(2, 4), input.indexOf(2, 4))
    assertEquals(expected.indexOf(2), input.indexOf(2))
  }
  @Test
  def segmentLength {
    val input = independentTraversable()
    val expected = independentExpectedData
    assertEquals(expected.segmentLength(lessThan(_, 10), 0), input.segmentLength(lessThan(_, 10)))
    assertEquals(expected.segmentLength(lessThan(_, 10), 3), input.segmentLength(lessThan(_, 10), 3))
  }
  @Test
  def prefixLength {
    val input = independentTraversable()
    val expected = independentExpectedData
    assertEquals(expected.prefixLength(lessThan(_, 10)), input.prefixLength(lessThan(_, 10)))
  }

  @Test
  def zipWithIndex {
    val input = independentTraversable()
    val expected = independentExpectedData

    assertEquals(expected.zipWithIndex.toList, input.zipWithIndex.toList)
  }
  @Test
  def startsWith {
    val input = independentTraversable()
    val expected = independentExpectedData
    assertTrue(input.startsWith(expected.take(55)))
    assertFalse(input.startsWith(0 to 55))
  }

  @Test
  def sliding {
    val input = independentTraversable()
    val expected = independentTraversable

    val basicsliding = input.sliding(10)
    assertEquals(expected.sliding(10).size, basicsliding.size)
    basicsliding.zip(expected.sliding(10).toSeq).forall {
      case (actual, expected) =>
        actual.toList == expected.toList
    }

    val skipsliding = input.sliding(10, 3)
    assertEquals(expected.sliding(10, 3).size, skipsliding.size)
    skipsliding.zip(expected.sliding(10, 3).toSeq).forall {
      case (actual, expected) =>
        actual.toList == expected.toList
    }
  }

}
