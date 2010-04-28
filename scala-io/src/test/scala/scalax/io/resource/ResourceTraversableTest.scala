/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io._
import Path.AccessModes._
import scalax.test.sugar._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import util.Random

import java.io._

class ResourceTraversableTest extends AssertionSugar with IOSugar{
  implicit val codec = Codec.UTF8
  def newResource[A](conv:Int=>Traversable[A] = (i:Int)=>List(i)) = {
      def stream = new ByteArrayInputStream(1 to 100 map {_.toByte} toArray)
      ResourceTraversable(Resource.fromInputStream(stream), _conv=conv)
    }
  val sample = Array(111,222)

  @Test @Ignore
  def should_handle_append = assertSizeAndType(newResource(), _ ++ sample)

  @Test @Ignore
  def size_should_work_like_lists = {
    val traversable = newResource()

    assertSizeAndType(traversable, t => t.slice(-1,300))
    assertSizeAndType(traversable, t => t.slice(0,0))
    assertSizeAndType(traversable, t => t.drop(300))
    assertSizeAndType(traversable, t => t.drop(-1))
    assertSizeAndType(traversable, t => t)
    assertSizeAndType(traversable, t => t.drop(0))
    assertSizeAndType(traversable, t => t.drop(0))
    assertSizeAndType(traversable, t => t.drop(2))
    assertSizeAndType(traversable, t => t.slice(2,10))
    assertSizeAndType(traversable, t => t.map{_.toChar}.slice(2,10))
    assertSizeAndType(traversable, t => t.slice(2,10).drop(5))
  }

  @Test @Ignore
  def should_handle_map = assertSizeAndType(newResource(), _ map {i => (i * -1).toString})

  @Test @Ignore
  def should_handle_several_ops = assertSizeAndType(newResource(), t => t ++ List(1,2) map {_.toString})

  @Test @Ignore
  def should_handle_++ = assertSizeAndType(newResource(), _ ++ List(1,2))
  
  @Test @Ignore
  def should_handle_drop = assertSizeAndType(newResource(), _ drop 2)
  
  @Test @Ignore
  def should_handle_drop_tomany = assertSizeAndType(newResource(), _ drop 10000)
  
  @Test @Ignore
  def should_handle_scanLeft_Right = {
    val r = newResource()
    assertSizeAndType(r, _.scanLeft(2){_ + _})
    assertSizeAndType(r, _.scanRight(2){_ + _})
  }

  @Test @Ignore
  def should_handle_collect = {
    val r = newResource()
    assertSizeAndType(r, _ collect {case i if i < 3 => i+1})
    assertSizeAndType(r, _ collect {case i if i < 3 => "a"+i})
  }

  @Test @Ignore
  def should_handle_flatMap = assertSizeAndType(newResource(), _ flatMap {i => 1 to 3 map {i + _}})

  @Test @Ignore
  def should_handle_flatten = {
    val traversable = newResource(i => List(List(i,i))) flatten
    val list = 1 to 100 map (i => List(i,i)) flatten

    assertEquals (list.size, traversable.size)
    assertEquals (list.toList, traversable.toList)      
  }

  @Test @Ignore
  def should_handle_dropWhile = assertSizeAndType(newResource(), _ dropWhile {_ < 55})
  
  @Test @Ignore
  def should_handle_filter = assertSizeAndType(newResource(), _ filter {_ > 55})
  
  @Test @Ignore
  def should_handle_filterNot = assertSizeAndType(newResource(), _ filterNot {_ > 55})
  
  @Test @Ignore
  def should_handle_groupBy = {
    def f(t : Traversable[Int]) = t groupBy {_.toString()(0)}
    val list = f(1 to 100 toList) 
    val applied = f(newResource()) map {case (k,v) => (k,v.toList)}

    assertEquals (list.size, applied.size)
    assertEquals (list.toList, applied.toList)
  }

  @Test @Ignore
  def should_handle_init = assertSizeAndType(newResource(), _ init)

  @Test @Ignore
  def should_handle_slice = assertSizeAndType(newResource(), _ slice(3,10))
  
  @Test @Ignore
  def should_handle_tail = assertSizeAndType(newResource(), _ tail)
  
  @Test @Ignore
  def should_handle_take = {
    assertSizeAndType(newResource(), _ take(10))
    assertSizeAndType(newResource(), _ take(-1))
    assertSizeAndType(newResource(), _ take(30000))
  }
  
  @Test @Ignore
  def should_handle_takeWhile = assertSizeAndType(newResource(), _ takeWhile {_ < 23})
  
  @Test @Ignore
  def should_handle_partition = {
    assertProductSizeAndType (newResource(), _ span {_ < 30})
    assertProductSizeAndType (newResource(), _ span {_ < -1})
  }
  @Test @Ignore
  def should_handle_splitAt = {
    assertProductSizeAndType (newResource(), _ splitAt 13)
    assertProductSizeAndType (newResource(), _ splitAt -1)
    assertProductSizeAndType (newResource(), _ splitAt 50000)
  }

  @Test @Ignore
  def should_handle_transpose = {
    val expected = (1 to 100).map (i => List(i,i+2,i+3)).transpose
    val actual = newResource(i => List(List(i,i+2,i+3))).transpose

    expected.zipWithIndex zip actual.toList foreach { 
      case ((expected:Traversable[_], index),actual:Traversable[_]) => 
        assertEquals (expected.size, actual.size)
        assertEquals (expected.toList, actual.toList)
    }
  }
  
  @Test @Ignore
  def should_handle_unzip = assertProductSizeAndType (newResource(), _.unzip(i => (1,i)))

  @Test @Ignore
  def flatmap_then_slice = assertSizeAndType(newResource(), t => t.flatMap{"x" + _}.slice(2,10))
  
  @Test @Ignore
  def map_then_slice = assertSizeAndType(newResource(), _ map {i => (i * -1).toString} slice (2,10))
  
  @Test @Ignore
  def drop_then_slice = assertSizeAndType(newResource(), _ map {i => (i * -1).toString} drop 3 slice (2,10))
  
  @Test @Ignore
  def filter_then_slice = assertSizeAndType(newResource(), _ map {i => (i * -1).toString} filter {_ < 45} slice (2,10))
  
  @Test @Ignore
  def slice_then_unzip = assertProductSizeAndType (newResource(), _ slice (3,10) unzip {i => (1,i)} )
  
  @Test //@Ignore
  def append_then_drop = 
    assertSizeAndType(newResource(), t => t ++ sample drop 98 )



  private def assertProductSizeAndType(traversable : Traversable[Int], f : Traversable[Int] => Product, areLongTraversable:Boolean = true) = {
    val list = f(1 to 100 toList)
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
    val list = f(1 to 100 toList)
    val applied = f(traversable)
    if(isLongTraversable)
      assert(applied.isInstanceOf[LongTraversable[_]], "new traversable is not a LongTraversable: "+applied.getClass.getName)
    assertEquals (list.size, applied.size)
    println(list mkString ",")
    println(applied mkString ",")

    assertEquals (list.toList, applied.toList)
  }

  def traversable_should_be_LongTraversable = {
    newResource().ltake(3L)  // if this compiles it passes the test
  }
   
}