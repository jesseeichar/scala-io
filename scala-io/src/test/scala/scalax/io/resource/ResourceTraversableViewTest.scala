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

class ResourceTraversableViewTest extends ResourceTraversableTest with AssertionSugar with IOSugar{

    override def newResource[A](conv:Int=>Traversable[A]) = super.newResource(conv).view
    
    @Test //@Ignore
    def map_should_not_trigger_resolution = {
      var count = 0
      newResource().map{i => count += 1;i.toString}
      assertEquals(0, count)
    }

    @Test //@Ignore
    def flatMap_should_not_trigger_resolution = {
      var count = 0
      newResource().flatMap{i => count += 1;i.toString}
      assertEquals(0, count)
    }

    @Test //@Ignore
    def foreach_on_drop_should_skip_dropped_elements = {
      var count = 0
      newResource().map{i => count += 1; i.toString}.drop(5).foreach{i => ()}
      assertEquals(95, count)
    }

/*
  TODO right now flatMap, append and filter are not optimized for laziness so all
  elements in traversable will be visited when a drop/slice/take are performed after
  one of those operations

    @Test //@Ignore
    def forcing_flatMap_should_trigger_resolution_and_skip_dropped_elements = {
      var count = 0
      newResource().flatMap{i => count += 1; i.toString}.drop(5).force
      assertEquals(95, count)
    }

    @Test //@Ignore
    def append_then_drop_should_skip_dropped_elements = {
      var count = 0
      ((newResource() map{i => count += 1; i}) ++ List(1,2,3) drop (5) force)
      assertEquals(95, count)
    }


    @Test //@Ignore
    def filter_then_drop_should_skip_dropped_elements = {
      var count = 0
      (newResource() map{i => count += 1; i} filter {_ <= 10} drop (5)).force
      assertEquals(5, count)
    }
*/
}