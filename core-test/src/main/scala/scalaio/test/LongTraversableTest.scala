package scalaio.test

import scalax.io._
import Input._
import scalax.test.sugar._

import org.junit.Assert._
import org.junit.{
Test, Before, After, Rule, Ignore
}

class LongTraversableTest {
  def traversable[U](tsize:Int)
                    (implicit callback:(Int)=>U = (_:Int) => (),
                     dataFunc: (Int) => Traversable[Int] = (i:Int) => 1 to i) =
    new LongTraversable[Int] {
      val data = dataFunc(tsize)
      def foreach[U](f: (Int) => U) = data foreach {i =>
        callback(i)
        f(i)
      }
    }

  @Test
  def map_is_lazy_on_view = {
    var check = 0

    val input = traversable(3)(_ => check += 1).view

    val mapped = input.map{ _ + 1}

    assertEquals(0,check)
    assertArrayEquals(Array(2,3,4),mapped.toArray)
  }

  @Test
  def limitedFold_only_processes_subset_of_elements = {
    var check = 1
    val input = traversable(100)(_ => check += 1)

    val calculation = input.limitFold(0) {
      case (acc,i) if i < 50 => Continue(i + acc)
      case (acc,i) => End(acc + i)
    }

    val expected = 1 to 50 reduceLeft (_ + _)

    assertEquals(51,check)
    assertEquals(expected, calculation)
  }

  @Test
  def apply{
    val input = traversable(100)
    1 to 100 foreach { i =>
      assertEquals(i,input(i-1))
    }
  }
  @Test
  def corresponds{
    val input = traversable(100)
    assertTrue(input.corresponds(1 to 100)( _ == _))
    assertFalse(input.corresponds(1 to 101)( _ == _))
    assertFalse(input.corresponds(0 to 100)( _ == _))
    assertFalse(input.corresponds(0 to 99)( _ == _))
    assertTrue(input.corresponds(1 to 100 map {_ => 300})( _ < _))
  }
  @Test
  def indexWhere{
    val input = traversable(100)
    assertEquals(2,input.indexWhere(_ == 3))
    assertEquals(6,input.indexWhere(_ > 3, 6))
    assertEquals(-1,input.indexWhere(_ > 300))
  }
  @Test
  def isDefinedAt{
    val input = traversable(100)
    assertEquals(2,input.indexWhere(_ == 3))
    assertEquals(6,input.indexWhere(_ > 3, 6))
    assertEquals(-1,input.indexWhere(_ > 300))
  }
  @Test
  def indexOf{
    val input = traversable(100)
    assertEquals(-1, input.indexOf(200))
    assertEquals(-1, input.indexOf(2,4))
    assertEquals(1, input.indexOf(2))
  }
  @Test
  def lastIndexOf{
    val input = traversable(0)(_ => (), _ => List(1,2,3,4,4,3,2,1))
    assertEquals(-1, input.lastIndexOf(200))
    assertEquals(7, input.lastIndexOf(1))
    assertEquals(0, input.lastIndexOf(1,3))
    assertEquals(2, input.lastIndexOf(3,2))
    assertEquals(4, input.lastIndexOf(4))
  }
  @Test
  def lastIndexWhere{
    val input = traversable(100)
    assertEquals(99, input.lastIndexWhere(200 >))
    assertEquals(20, input.lastIndexWhere(200 >,20))
    assertEquals(-1, input.lastIndexWhere(1000 <))
    assertEquals(1, input.lastIndexWhere(2 ==))
  }
  @Test
  def segmentLength{
    val input = traversable(100)
    assertEquals(9, input.segmentLength(10 >))
    assertEquals(9 - 3, input.segmentLength(10 >,3))
    assertEquals(0, input.segmentLength(100 <))
  }
  @Test
  def prefixLength{
    val input = traversable(100)
    assertEquals(9, input.prefixLength(10 >))
    assertEquals(0, input.prefixLength(100 <))
  }
  @Test
  def startsWith {
    val input = traversable(100)
    assertTrue(input.startsWith(1 to 55))
    assertFalse(input.startsWith(0 to 55))
  }
  @Test
  def indexOfSlice{
    val input = traversable(100)
    assertEquals(0,input.indexOfSlice(1 to 55))
    assertEquals(2,input.indexOfSlice(3 to 55,2))
    assertEquals(39,input.indexOfSlice(40 to 55,30))
    assertEquals(-1,input.indexOfSlice(1 to 55,30))
    assertEquals(-1,input.indexOfSlice(1 to 101,0))
    assertEquals(0,input.indexOfSlice(Nil,0))
  }
  @Test
  def containsSlice{
    val input = traversable(100)
    assertTrue(input.containsSlice(Nil))
    assertTrue(input.containsSlice(1 to 55))
    assertTrue(input.containsSlice(3 to 55,2))
    assertTrue(input.containsSlice(40 to 55,30))
    assertFalse(input.containsSlice(1 to 55,30))
    assertFalse(input.containsSlice(1 to 101,0))

  }



}