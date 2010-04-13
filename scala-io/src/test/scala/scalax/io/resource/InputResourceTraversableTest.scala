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

    def resource = Resource.fromInputStream(new ByteArrayInputStream(1 to 100 map {_.toByte} toArray))

    val sample = Array(111,222)

    @Test
    def should_handle_append = {
      val patched = ResourceTraversable(resource) ++ sample
      assert (patched.isInstanceOf[ResourceTraversable[_,_]], "new Traversable is not an ResourceTraversable")
      assert (patched.size == 102)
      assert (patched.drop(100).toArray == sample)
    }

    @Test
    def should_handle_map = {
      val strings = ResourceTraversable(resource) map {_.toString}
      assert (strings.isInstanceOf[ResourceTraversable[_,_]], "new Traversable is not an ResourceTraversable")
      assert (strings.head == "1")
    }
    
    @Test
    def should_handle_drop = {
      val dropped = ResourceTraversable(resource) drop(2)
      assert (dropped.isInstanceOf[ResourceTraversable[_,_]], "new Traversable is not an ResourceTraversable")
    }
    
    @Test
    def should_handle_drop_tomany = {
      val dropped = ResourceTraversable(resource) drop(10000)
      assert (dropped.isInstanceOf[ResourceTraversable[_,_]], "new Traversable is not an ResourceTraversable")
      assert (dropped.headOption.isEmpty)
    }
    
    /*
    scanLeft
    scanRight
    collect
    flatMap
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
}