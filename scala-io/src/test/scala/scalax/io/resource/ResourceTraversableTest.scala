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

    @Test //@Ignore
    def should_handle_append = assertSizeAndType(newResource(), _ ++ sample)

    @Test //@Ignore
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

    @Test
    def slice_and_flatmap = assertSizeAndType(newResource(), t => t.flatMap{"_"+_.toChar}.slice(2,10))
    
    @Test //@Ignore
    def should_handle_map = assertSizeAndType(newResource(), _ map {i => (i * -1).toString})

    @Test //@Ignore
    def should_handle_several_ops = assertSizeAndType(newResource(), t => t ++ List(1,2) map {_.toString})
    
    @Test //@Ignore
    def should_handle_drop = assertSizeAndType(newResource(), _ drop 2)
    
    @Test //@Ignore
    def should_handle_drop_tomany = assertSizeAndType(newResource(), _ drop 10000)
    
    @Test //@Ignore
    def should_remain_traversable_after_several_operations = 
      assertSizeAndType(newResource(), t => t ++ sample drop 100 )
    
    @Test //@Ignore
    def should_handle_scanLeft_Right = {
      val r = newResource()
      assertSizeAndType(r, _.scanLeft(2){_ + _})
      assertSizeAndType(r, _.scanRight(2){_ + _})
    }

    @Test //@Ignore
    def should_handle_collect = {
      val r = newResource()
      assertSizeAndType(r, _ collect {case i if i < 3 => i+1})
      assertSizeAndType(r, _ collect {case i if i < 3 => "a"+i})
    }

    @Test //@Ignore
    def should_handle_flatMap = assertSizeAndType(newResource(), _ flatMap {i => 1 to 3 map {i + _}})

    //@Test //@Ignore
    // def should_handle_flatten = {
    //   assertSizeAndType(newResource(), _ flatMap {i => 1 to 3 map {i + _}}, )
    // }
    /*
    test laziness
    flatten
    dropWhile
    filter
    filterNot
    groupBy
    init
    partition
    repr
    slice
    span
    splitAt
    tail
    take
    takeWhile
    dropWhile
    transpose
    unzip
    */
    
    
    private def assertSizeAndType[A](traversable : Traversable[Int], f : Traversable[Int] => Traversable[A], init : List[Int] = (1 to 100 toList)) = {
      val list : Traversable[A] = f(init)
      val applied : LongTraversable[A] = f(traversable).asInstanceOf[LongTraversable[A]]
      assertEquals (list.size, applied.size)
      assertEquals (list.toList, applied.toList)
    }
 
    def traversable_should_be_LongTraversable = {
      newResource().ltake(3L)  // if this compiles it passes the test
    }
   
}