package scalaio.test

import org.junit.Assert._
import org.junit.Test

import scalax.io.JavaConverters._
import scalax.io._
import scalax.test.sugar._

class LongTraversableTest extends DataIndependentLongTraversableTest[Int] with TransformationTest with AssertionSugar {
  implicit val codec = Codec.UTF8

  def independentTraversable(): LongTraversable[Int] = traversable()
  val identity = (i: Int) => i
  def traversable[U, A](tsize: Int = 100,
    callback: (Int) => U = (_: Int) => (),
    dataFunc: (Int) => Traversable[Int] = (i: Int) => 1 to i,
    conv: Int => A = identity): LongTraversable[A] = {
    val lt = new LongTraversable[Int] {
      val data = dataFunc(tsize)

      def iterator: CloseableIterator[Int] = new CloseableIterator[Int] {
        val iter = data.toIterator
        def next(): Int = {
          val i = iter.next()
          callback(i)
          i
        }
        def hasNext: Boolean = iter.hasNext
        def doClose() {}
      }
    }
    if (conv != identity) lt.map(conv)
    else lt.asInstanceOf[LongTraversable[A]]
  }

  val sample = Array(111, 222)

  def independentExpectedData = expectedData()
  protected def expectedData(tsize: Int = 100,
    dataFunc: (Int) => Seq[Int] = (i: Int) => 1 to i) = dataFunc(tsize)

  def times(t1: Int, t2: Int) = t1 * t2
  def lessThan(t: Int, i: Int): Boolean = t < i
  def scanSeed = 2
  @Test //@Ignore
  def should_handle_flatMap = assertSizeAndType(traversable(), _ flatMap { i => 1 to 3 map { i + _ } })

  @Test //@Ignore
  def should_handle_flatten = {
    val t = traversable(conv = (i: Int) => List(i, i)).flatten
    val list = expectedData() map (i => List(i, i)) flatten

    assertEquals(list.size, t.size)
    assertEquals(list.toList, t.toList)
  }

  @Test //@Ignore
  def should_handle_transpose = {
    val expected = (expectedData().toList).map(i => List(i, i + 2, i + 3)).transpose
    val actual = traversable(conv = i => List(i, i + 2, i + 3)).transpose

    expected.zipWithIndex zip actual.toList foreach {
      case ((expected: Traversable[_], index), actual: Traversable[_]) =>
        assertEquals(expected.size, actual.size)
        assertEquals(expected.toList, actual.toList)
    }
  }

  @Test
  def map_is_lazy = {
    var check = 0

    val input = traversable(tsize = 3, callback = (i: Int) => check += 1)

    val mapped = input.map { _ + 1 }

    assertEquals(0, check)
    assertArrayEquals(expectedData(3).map { _ + 1 }.toArray, mapped.toArray)
  }

  @Test
  def limitedFold_only_processes_subset_of_elements = {
    var check = 1
    val input = traversable(tsize = 100, callback = _ => check += 1)

    var count = 1

    val calculation = input.limitFold(0) {
      case (acc, i) if count < 50 =>
        count += 1
        Continue(i + acc)
      case (acc, i) => End(acc + i)
    }

    val expected = expectedData().take(50) reduceLeft (_ + _)

    assertEquals(51, check)
    assertEquals(expected, calculation)
  }

  @Test
  def corresponds {
    val input = traversable()
    assertTrue(input.corresponds(expectedData())(_ == _))
    assertFalse(input.corresponds(expectedData(dataFunc = _ => 1 to 101))(_ == _))
    assertFalse(input.corresponds(expectedData(dataFunc = _ => 0 to 100))(_ == _))
    assertFalse(input.corresponds(expectedData(dataFunc = _ => 0 to 99))(_ == _))
    assertTrue(input.corresponds(expectedData(dataFunc = _ => 1 to 100) map { _ => 300 })(_ < _))

    assertTrue(input.corresponds(toLongResource(expectedData()))(_ == _))
    assertFalse(input.corresponds(toLongResource(expectedData(dataFunc = _ => 1 to 101)))(_ == _))
    assertFalse(input.corresponds(toLongResource(expectedData(dataFunc = _ => 0 to 100)))(_ == _))
    assertFalse(input.corresponds(toLongResource(expectedData(dataFunc = _ => 0 to 99)))(_ == _))
    assertTrue(input.corresponds(toLongResource(expectedData(dataFunc = _ => 1 to 100) map { _ => 300 }))(_ < _))
  }
  @Test
  def indexWhere {
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.indexWhere(_ == 3), input.indexWhere(_ == 3))
    assertEquals(expected.indexWhere(_ > 3, 6), input.indexWhere(_ > 3, 6))
    assertEquals(expected.indexWhere(_ > 300), input.indexWhere(_ > 300))
  }
  def lastIndexOf {
    val func = (_: Int) => List(1, 2, 3, 4, 4, 3, 2, 1)
    val input = traversable(tsize = 0, callback = _ => (), dataFunc = func)
    val expected = expectedData(0, func)
    assertEquals(expected.lastIndexOf(200), input.lastIndexOf(200))
    assertEquals(expected.lastIndexOf(1), input.lastIndexOf(1))
    assertEquals(expected.lastIndexOf(1, 3), input.lastIndexOf(1, 3))
    assertEquals(expected.lastIndexOf(3, 2), input.lastIndexOf(3, 2))
    assertEquals(expected.lastIndexOf(4), input.lastIndexOf(4))
  }

  @Test
  def lastIndexWhere {
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.lastIndexWhere(200 >), input.lastIndexWhere(200 >))
    assertEquals(expected.lastIndexWhere(200 >, 20), input.lastIndexWhere(200 >, 20))
    assertEquals(expected.lastIndexWhere(1000 <), input.lastIndexWhere(1000 <))
    assertEquals(expected.lastIndexWhere(2 ==), input.lastIndexWhere(2 ==))
  }

  @Test
  def indexOfSlice {
    val input = traversable()
    val expected = expectedData(101)

    assertEquals(0, input.indexOfSlice(expected.take(55)))
    assertEquals(2, input.indexOfSlice(expected.slice(2, 55), 2))
    assertEquals(39, input.indexOfSlice(expected.slice(39, 55), 30))
    assertEquals(-1, input.indexOfSlice(expected.take(55), 30))
    assertEquals(-1, input.indexOfSlice(expected))
    assertEquals(0, input.indexOfSlice(Nil, 0))
  }
  @Test
  def containsSlice {
    val input = traversable()
    val expected = expectedData(101)

    assertTrue(input.containsSlice(Nil))
    assertTrue(input.containsSlice(expected take 55))
    assertTrue(input.containsSlice(expected slice (2, 55), 2))
    assertTrue(input.containsSlice(expected slice (39, 55), 30))
    assertFalse(input.containsSlice(expected take 55, 30))
    assertFalse(input.containsSlice(expected, 0))
  }
  @Test
  def segmentLength2 {
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.segmentLength(100 <, 0), input.segmentLength(100 <))
  }
  @Test
  def prefixLength2 {
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.prefixLength(100 <), input.prefixLength(100 <))
  }
  @Test
  def sameContents {
    val input = traversable()
    val expected = expectedData(101)

    assertFalse(input.sameElements(Nil))
    assertFalse(input.sameElements(expected))
    assertFalse(input.sameElements(expected take 1))
    assertTrue(traversable(3).sameElements(expectedData(3)))
    assertTrue(traversable(20).sameElements(expectedData(20)))
    assertTrue(input.take(3).sameElements(expected take 3))
    assertTrue(input.take(100).sameElements(expected take 100))

    assertFalse(input.sameElements(toLongResource(Nil)))
    assertFalse(input.sameElements(toLongResource(expected)))
    assertFalse(input.sameElements(toLongResource(expected take 1)))
    assertTrue(traversable(3).sameElements(toLongResource(expectedData(3))))
    assertTrue(traversable(20).sameElements(toLongResource(expectedData(20))))
    assertTrue(input.take(3).sameElements(toLongResource(expected take 3)))
    assertTrue(input.take(100).sameElements(toLongResource(expected take 100)))
  }

  @Test
  def zip {
    val input = traversable()
    val expected = expectedData()

    assertEquals(expected.zip(1 to expected.size).toList,
      input.zip(1 to expected.size).toList)

    assertEquals(expected.zip(1 to expected.size).toList,
      input.zip(toLongResource(1 to expected.size)).toList)

  }
  @Test
  def zip_is_lazy {
    var count = 0
    val input = traversable(callback = _ => count += 1)

    val range = 1 to 100
    input.zip(range)
    assertEquals(0, count)

    input.zip(traversable(100))
    assertEquals(0, count)
  }

  @Test
  def zipAll {
    val input = traversable()
    val expected = expectedData()

    assertEquals(expected.zipAll(1 to (expected.size * 2), 1, 2).toList,
      input.zipAll(1 to (expected.size * 2), 1, 2).toList)

    assertEquals(expected.zipAll(1 to (expected.size * 2), 1, 2).toList,
      input.zipAll(toLongResource(1 to (expected.size * 2)), 1, 2).toList)
  }
  @Test
  def zipAll_is_lazy {
    var count = 0
    val input = traversable(callback = _ => count += 1)

    input.zipAll(1 to 130, 2, 3)
    assertEquals(0, count)
    input.zipAll(traversable(130), 2, 3)
    assertEquals(0, count)
  }
  @Test
  def zipWithIndex_is_lazy {
    var count = 0
    val input = traversable(callback = _ => count += 1)

    input.zipWithIndex
    assertEquals(0, count)
  }
  @Test
  def sliding_is_lazy {
    var count = 0
    val input = traversable(callback = _ => count += 1)

    input.sliding(10, 1)
    assertEquals(0, count)
  }
  @Test
  def grouped_is_lazy {
    var count = 0
    val input = traversable(callback = _ => count += 1)

    input.grouped(10)
    assertEquals(0, count)
  }
  def toLongResource[A](wrappedSeq: Seq[A]): LongTraversable[A] = new LongTraversable[A] {
    def iterator: CloseableIterator[A] = CloseableIterator(wrappedSeq.iterator)
  }

  @Test //@Ignore
  def map_should_not_trigger_resolution = {
    var count = 0
    traversable().map { i => count += 1; i.toString }
    assertEquals(0, count)
  }

  @Test //@Ignore
  def flatMap_should_not_trigger_resolution = {
    var count = 0
    traversable().flatMap { i => count += 1; i.toString }
    assertEquals(0, count)
  }

  @Test //@Ignore
  def foreach_on_drop_should_skip_dropped_elements = assertLazy(_.drop(5))
  @Test //@Ignore
  def init_is_lazy = assertLazy(_.init)
  @Test //@Ignore
  def filter_is_lazy = assertLazy(_.filter(_ < 30))
  @Test //@Ignore
  def slice_is_lazy = assertLazy(_.slice(3, 5))
  @Test //@Ignore
  def dropWhile_is_lazy = assertLazy(_.dropWhile(_ < 30))
  @Test //@Ignore
  def takeWhile_is_lazy = assertLazy(_.takeWhile(_ < 30))

  @Test //@Ignore
  def partition_is_lazy = {
    var count = 0
    val t = traversable(callback = _ => count += 1)
    t.partition(_ < 30)
    assertEquals(0, count)
  }

  @Test //@Ignore
  def slitAt_is_lazy = {
    var count = 0
    val t = traversable(callback = _ => count += 1)
    t.splitAt(4)
    assertEquals(0, count)
  }

  @Test //@Ignore
  def span_is_lazy = {
    var count = 0
    val t = traversable(callback = _ => count += 1)
    t.span(_ < 30)
    assertEquals(0, count)
  }

  @Test //@Ignore
  def collect_is_lazy = assertLazy(_.collect {
    case i if i < 10 => 'c'
    case j if j >= 10 => 'f'
  })

  private def assertLazy[A](f: Traversable[Int] => Traversable[A], isLongTraversable: Boolean = true) = {
    var expectedCount = 0
    var count = 0
    val list = (f(expectedData().toList.view) map { i => expectedCount += 1; i })

    var callbackCount = 0
    val traversableObj = traversable(callback = _ => callbackCount += 1)
    val applied = (f(traversableObj).map { i => count += 1; i })
    if (isLongTraversable)
      assert(applied.isInstanceOf[LongTraversable[_]], "new traversable is not a LongTraversable: " + applied)

    assertEquals(0, callbackCount)
    // force all elements to be processed
    list.foreach(i => ())
    applied.foreach(i => ())

    assertEquals(expectedCount, count)
  }
   @Test
  def exception_thrown_if_iterator_escapes_withIterator {
    intercept[AssertionError] {
      traversable().withIterator { i => i }
    }
  }
 
}