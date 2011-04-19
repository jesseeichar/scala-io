package scalaio.test

import scalax.io._
import Input._
import scalax.test.sugar._

import org.junit.Assert._
import org.junit.{
Test, Before, After, Rule, Ignore
}

class LongTraversableTest {
  implicit val codec = Codec.UTF8

  val identity = (i:Int) => i
  def traversable[U,A](tsize:Int = 100,
                     callback:(Int)=>U = (_:Int) => (),
                     dataFunc: (Int) => Traversable[Int] = (i:Int) => 1 to i,
                     conv:Int=>A = identity
                    ):LongTraversable[A] = {
    val lt=new LongTraversable[Int] {
      val data = dataFunc(tsize)

      def iterator: CloseableIterator[Int] = new CloseableIterator[Int] {
        val iter = data.toIterator
        def next(): Int = {
          val i = iter.next()
          callback(i)
          i
        }
        def hasNext: Boolean = iter.hasNext
        def close() {}
      }
    }
    if(conv != identity) lt.map(conv)
    else lt.asInstanceOf[LongTraversable[A]]
  }


  val sample = Array(111,222)

  protected def expectedData(tsize:Int=100,
                             dataFunc: (Int) => Seq[Int] = (i:Int) => 1 to i)
    = dataFunc(tsize)

  @Test //@Ignore
  def should_handle_append = assertSizeAndType(traversable(), _ ++ sample)

  @Test //@Ignore
  def size_should_work_like_lists = {
    val traversable2 = traversable()

    assertSizeAndType(traversable2, t => t.slice(-1,300))
    assertSizeAndType(traversable2, t => t.slice(0,0))
    assertSizeAndType(traversable2, t => t.drop(300))
    assertSizeAndType(traversable2, t => t.drop(-1))
    assertSizeAndType(traversable2, t => t)
    assertSizeAndType(traversable2, t => t.drop(0))
    assertSizeAndType(traversable2, t => t.drop(0))
    assertSizeAndType(traversable2, t => t.drop(2))
    assertSizeAndType(traversable2, t => t.slice(2,10))
    assertSizeAndType(traversable2, t => t.map{_.toChar}.slice(2,10))
    assertSizeAndType(traversable2, t => t.take(10).drop(5))
    assertSizeAndType(traversable2, t => t.take(10).drop(5).take(2))
    assertSizeAndType(traversable2, t => t.drop(10).take(5).drop(20))
    assertSizeAndType(traversable2, t => t.drop(10).drop(5).drop(20))
    assertSizeAndType(traversable2, t => t.slice(2,10).slice(1,5))
    assertSizeAndType(traversable2, t => t.slice(2,10).drop(5))
  }

  @Test //@Ignore
  def should_handle_map = assertSizeAndType(traversable(), _ map {i => (i * -1).toString})

  @Test //@Ignore
  def should_handle_several_ops = assertSizeAndType(traversable(), t => t ++ List(1,2) map {_.toString})

  @Test //@Ignore
  def should_handle_++ = assertSizeAndType(traversable(), _ ++ List(1,2))

  @Test //@Ignore
  def should_handle_drop = assertSizeAndType(traversable(), _ drop 2)

  @Test //@Ignore
  def should_handle_drop_tomany = assertSizeAndType(traversable(), _ drop 10000)

  @Test //@Ignore
  def should_handle_scanLeft_Right = {
    val r = traversable()
    assertSizeAndType(r, _.scanLeft(2){_ + _})
    assertSizeAndType(r, _.scanRight(2){_ + _})
  }

  @Test //@Ignore
  def should_handle_collect = {
    val r = traversable()
    assertSizeAndType(r, _ collect {case i if i < 3 => i+1})
    assertSizeAndType(r, _ collect {case i if i < 3 => "a"+i})
  }

  @Test //@Ignore
  def should_handle_flatMap = assertSizeAndType(traversable(), _ flatMap {i => 1 to 3 map {i + _}})

  @Test //@Ignore
  def should_handle_flatten = {
    val t = traversable(conv = (i:Int) => List(i,i)).flatten
    val list = expectedData() map (i => List(i,i)) flatten

    assertEquals (list.size, t.size)
    assertEquals (list.toList, t.toList)
  }

  @Test //@Ignore
  def should_handle_dropWhile = assertSizeAndType(traversable(), _ dropWhile {_ < 55})

  @Test //@Ignore
  def should_handle_filter = assertSizeAndType(traversable(), _ filter {_ > 55})

  @Test //@Ignore
  def should_handle_filterNot = assertSizeAndType(traversable(), _ filterNot {_ > 55})

  @Test //@Ignore
  def should_handle_groupBy = {
    def f(t : Traversable[Int]) = t groupBy {_.toString()(0)}
    val list = f(expectedData() toList)
    val applied = f(traversable()) map {case (k,v) => (k,v.toList)}

    assertEquals (list.size, applied.size)
    assertEquals (list.toList, applied.toList)
  }

  @Test //@Ignore
  def should_handle_init = assertSizeAndType(traversable(), _ init, false)

  @Test //@Ignore
  def should_handle_slice = assertSizeAndType(traversable(), _ slice(3,10))

  //@Test //@Ignore
  //def should_handle_zip = assertSizeAndType(traversable(), _ zip (101 to 200))

  @Test //@Ignore
  def should_handle_tail = assertSizeAndType(traversable(), _ tail)

  @Test //@Ignore
  def should_handle_take = {
    assertSizeAndType(traversable(), _ take(10))
    assertSizeAndType(traversable(), _ take(-1))
    assertSizeAndType(traversable(), _ take(30000))
  }

  @Test //@Ignore
  def should_handle_takeWhile = assertSizeAndType(traversable(), _ takeWhile {_ < 23})

  @Test //@Ignore
  def should_handle_partition = {
    assertProductSizeAndType (traversable(), _ span {_ < 30})
    assertProductSizeAndType (traversable(), _ span {_ < -1})
  }
  @Test //@Ignore
  def should_handle_splitAt = {
    assertProductSizeAndType (traversable(), _ splitAt 13,false)
    assertProductSizeAndType (traversable(), _ splitAt -1,false)
    assertProductSizeAndType (traversable(), _ splitAt 50000,false)
  }

  @Test //@Ignore
  def should_handle_transpose = {
    val expected = (expectedData().toList).map (i => List(i,i+2,i+3)).transpose
    val actual = traversable(conv = i => List(i,i+2,i+3)).transpose

    expected.zipWithIndex zip actual.toList foreach {
      case ((expected:Traversable[_], index),actual:Traversable[_]) =>
        assertEquals (expected.size, actual.size)
        assertEquals (expected.toList, actual.toList)
    }
  }

  @Test //@Ignore
  def should_handle_unzip = assertProductSizeAndType (traversable(), _.unzip(i => (1,i)))

  @Test //@Ignore
  def flatmap_then_slice = assertSizeAndType(traversable(), t => t.flatMap{"x" + _}.slice(2,10))

  @Test //@Ignore
  def map_then_slice = assertSizeAndType(traversable(), _ map {i => (i * -1).toString} slice (2,10))

  @Test //@Ignore
  def map_then_take = assertSizeAndType(traversable(), _ map {i => (i * -1).toString} take 10)

  @Test //@Ignore
  def drop_then_slice = assertSizeAndType(traversable(), _ map {i => (i * -1).toString} drop 3 slice (2,10))

  @Test //@Ignore
  def filter_then_slice = assertSizeAndType(traversable(), _ filter {_ < 45} slice (2,10))

  @Test //@Ignore
  def filter_then_take = assertSizeAndType(traversable(), _ filter {_ < 45} take 10)

  @Test //@Ignore
  def filter_then_drop = assertSizeAndType(traversable(), _ filter {_ < 45} drop 10)

  @Test //@Ignore
  def drop_then_take = assertSizeAndType(traversable(), _ drop 10 take 10)

  @Test //@Ignore
  def drop_then_drop = assertSizeAndType(traversable(), _ drop 45 drop 10)

  @Test //@Ignore
  def drop_then_append = assertSizeAndType(traversable(), t => (t drop 45) ++ List(1,2,3))

  @Test //@Ignore
  def drop_to_many = assertSizeAndType(traversable(), t => (t drop 1000))

  @Test //@Ignore
  def slice_to_0 = assertSizeAndType(traversable(), t => (t slice (5,10) slice (0,-3)))

  @Test //@Ignore
  def slice_then_unzip = assertProductSizeAndType (traversable(), _ slice (3,10) unzip {i => (1,i)} )

  @Test //@Ignore
  def append_then_drop =
    assertSizeAndType(traversable(), t => t ++ sample drop 98 )

    @Test //@Ignore
    def size = assertFalse(traversable().hasDefiniteSize)

  private def assertProductSizeAndType(traversable : Traversable[Int], f : Traversable[Int] => Product, areLongTraversable:Boolean = true) = {
    val list = f(expectedData() toList)
    val applied = f(traversable)
    if(areLongTraversable) {
      applied.productIterator foreach {t => assert(t.isInstanceOf[LongTraversable[_]], "new traversable is not a LongTraversable: "+applied.getClass.getName)}
    }
    assertEquals (list.productArity, applied.productArity)

    list.productIterator.zipWithIndex zip applied.productIterator foreach {
      case ((expected:Traversable[_], index),actual:Traversable[_]) =>
        assertEquals (expected.size, actual.size)
        assertEquals (expected.toList, actual.toList)
    }
  }

  private def assertSizeAndType[A](traversable : Traversable[Int], f : Traversable[Int] => Traversable[A], isLongTraversable:Boolean = true) = {
    val list = f(expectedData().toList)
    val applied = f(traversable)
    if(isLongTraversable)
      assert(applied.isInstanceOf[LongTraversable[_]], "new traversable is not a LongTraversable: "+applied.getClass.getName)
    assertEquals (list.size, applied.size)

    assertEquals (list.toList, applied.toList)
  }

  def traversable_should_be_LongTraversable = {
    traversable().ltake(3L)  // if this compiles it passes the test
  }

  @Test
  def map_is_lazy_on_view = {
    var check = 0

    val input = traversable(tsize=3,callback= (i:Int) => check += 1).view

    val mapped = input.map{ _ + 1}

    assertEquals(0,check)
    assertArrayEquals(expectedData(3).map{_ + 1}.toArray,mapped.toArray)
  }

  @Test
  def limitedFold_only_processes_subset_of_elements = {
    var check = 1
    val input = traversable(tsize=100,callback=_ => check += 1)

    var count = 1

    val calculation = input.limitFold(0) {
      case (acc,i) if count < 50 =>
        count += 1
        Continue(i + acc)
      case (acc,i) => End(acc + i)
    }

    val expected = expectedData().take(50) reduceLeft (_ + _)

    assertEquals(51,check)
    assertEquals(expected, calculation)
  }

  @Test
  def apply{
    val input = traversable()
    val expected = expectedData()
    0 until expected.size foreach { i =>
      assertEquals(expected(i),input(i))
    }
  }

  @Test
  def ldrop{
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.drop(10).size,input.ldrop(10).size)
  }
  @Test
  def ltake{
    val input = traversable()
    val expected = expectedData()
    assertEquals((expected take 10).size,input.ltake(10).size)
  }
  @Test
  def lslice{
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.slice(2,10).size,input.lslice(2,10).size)
    assertEquals(expected.slice(11,10).size,input.lslice(11,10).size)
    assertEquals(expected.take(10).size,input.lslice(-1,10).size)
    assertEquals(expected.take(10).size,input.lslice(-1,10).lslice(-1,10).size)
    assertEquals(expected.slice(2,Int.MaxValue).size,input.lslice(2,Long.MaxValue).size)
  }
  @Test
  def corresponds{
    val input = traversable()
    assertTrue(input.corresponds(expectedData())( _ == _))
    assertFalse(input.corresponds(expectedData(dataFunc = _ => 1 to 101))( _ == _))
    assertFalse(input.corresponds(expectedData(dataFunc = _ => 0 to 100))( _ == _))
    assertFalse(input.corresponds(expectedData(dataFunc = _ => 0 to 99))( _ == _))
    assertTrue(input.corresponds(expectedData(dataFunc = _ => 1 to 100) map {_=> 300})( _ < _))

    assertTrue(input.corresponds(toLongResource(expectedData()))( _ == _))
    assertFalse(input.corresponds(toLongResource(expectedData(dataFunc = _ => 1 to 101)))( _ == _))
    assertFalse(input.corresponds(toLongResource(expectedData(dataFunc = _ => 0 to 100)))( _ == _))
    assertFalse(input.corresponds(toLongResource(expectedData(dataFunc = _ => 0 to 99)))( _ == _))
    assertTrue(input.corresponds(toLongResource(expectedData(dataFunc = _ => 1 to 100) map {_=> 300}))( _ < _))
  }
  @Test
  def indexWhere{
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.indexWhere(_ == 3),input.indexWhere(_ == 3))
    assertEquals(expected.indexWhere(_ > 3, 6),input.indexWhere(_ > 3, 6))
    assertEquals(expected.indexWhere(_ > 300),input.indexWhere(_ > 300))
  }
  @Test
  def isDefinedAt{
    val input = traversable()
    assertTrue(input.isDefinedAt(5))
    assertFalse(input.isDefinedAt(-1))
    assertFalse(input.isDefinedAt(300))
  }

  @Test
  def indexOf{
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.indexOf(200), input.indexOf(200))
    assertEquals(expected.indexOf(2,4), input.indexOf(2,4))
    assertEquals(expected.indexOf(2), input.indexOf(2))
  }
  @Test
  def lastIndexOf{
    val func = (_:Int) => List(1,2,3,4,4,3,2,1)
    val input = traversable(tsize=0,callback=_ => (), dataFunc= func)
    val expected = expectedData(0,func)
    assertEquals(expected.lastIndexOf(200), input.lastIndexOf(200))
    assertEquals(expected.lastIndexOf(1), input.lastIndexOf(1))
    assertEquals(expected.lastIndexOf(1,3), input.lastIndexOf(1,3))
    assertEquals(expected.lastIndexOf(3,2), input.lastIndexOf(3,2))
    assertEquals(expected.lastIndexOf(4), input.lastIndexOf(4))
  }
  @Test
  def lastIndexWhere{
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.lastIndexWhere(200 >), input.lastIndexWhere(200 >))
    assertEquals(expected.lastIndexWhere(200 >,20), input.lastIndexWhere(200 >,20))
    assertEquals(expected.lastIndexWhere(1000 <), input.lastIndexWhere(1000 <))
    assertEquals(expected.lastIndexWhere(2 ==), input.lastIndexWhere(2 ==))
  }
  @Test
  def segmentLength{
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.segmentLength(10 >,0), input.segmentLength(10 >))
    assertEquals(expected.segmentLength(10 >,3), input.segmentLength(10 >,3))
    assertEquals(expected.segmentLength(100 <,0), input.segmentLength(100 <))
  }
  @Test
  def prefixLength{
    val input = traversable()
    val expected = expectedData()
    assertEquals(expected.prefixLength(10 >), input.prefixLength(10 >))
    assertEquals(expected.prefixLength(100 <), input.prefixLength(100 <))
  }
  @Test
  def startsWith {
    val input = traversable()
    val expected = expectedData()
    assertTrue(input.startsWith(expected.take(55)))
    assertFalse(input.startsWith(0 to 55))
  }
  @Test
  def indexOfSlice{
    val input = traversable()
    val expected = expectedData(101)

    assertEquals(0,input.indexOfSlice(expected.take(55)))
    assertEquals(2,input.indexOfSlice(expected.slice(2,55),2))
    assertEquals(39,input.indexOfSlice(expected.slice(39,55),30))
    assertEquals(-1,input.indexOfSlice(expected.take(55),30))
    assertEquals(-1,input.indexOfSlice(expected))
    assertEquals(0,input.indexOfSlice(Nil,0))
  }
  @Test
  def containsSlice{
    val input = traversable()
    val expected = expectedData(101)

    assertTrue(input.containsSlice(Nil))
    assertTrue(input.containsSlice(expected take 55))
    assertTrue(input.containsSlice(expected slice (2,55),2))
    assertTrue(input.containsSlice(expected slice (39,55),30))
    assertFalse(input.containsSlice(expected take 55,30))
    assertFalse(input.containsSlice(expected,0))
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

    assertFalse(input.sameElements(toLongResource (Nil)))
    assertFalse(input.sameElements(toLongResource (expected)))
    assertFalse(input.sameElements(toLongResource (expected take 1)))
    assertTrue(traversable(3).sameElements(toLongResource (expectedData(3))))
    assertTrue(traversable(20).sameElements(toLongResource (expectedData(20))))
    assertTrue(input.take(3).sameElements(toLongResource (expected take 3)))
    assertTrue(input.take(100).sameElements(toLongResource (expected take 100)))
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
    val input = traversable(callback = _ => count += 1).view

    input.zip(1 to 100)
    assertEquals(0,count)
  }
  @Test
  def zipAll_is_lazy {
    var count = 0
    val input = traversable(callback = _ => count += 1).view

    input.zipAll(1 to 130,2,3)
    assertEquals(0,count)
  }
  @Test
  def zipWithIndex_is_lazy {
    var count = 0
    val input = traversable(callback = _ => count += 1).view

    input.zipWithIndex
    assertEquals(0,count)
  }

  @Test
  def zipAll {
    val input = traversable()
    val expected = expectedData()

    assertEquals(expected.zipAll(1 to (expected.size * 2), 1,2).toList,
      input.zipAll(1 to (expected.size * 2), 1,2).toList)

    assertEquals(expected.zipAll(1 to (expected.size * 2), 1,2).toList,
      input.zipAll(toLongResource (1 to (expected.size * 2)), 1,2).toList)
  }

  @Test
  def zipWithIndex {
    val input = traversable()
    val expected = expectedData()

    assertEquals(expected.zipWithIndex.toList, input.zipWithIndex.toList)
  }

  def toLongResource[A](wrappedSeq:Seq[A]):LongTraversable[A] = new LongTraversable[A]{
    def iterator: CloseableIterator[A] = CloseableIterator(wrappedSeq.iterator)
  }
}