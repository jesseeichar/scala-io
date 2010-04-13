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

class InputResourceTraversableTest extends AssertionSugar with IOSugar{
    implicit val codec = Codec.UTF8

    def resource = Resource.fromInputStream(new ByteArrayInputStream(1 to 100 toArray))

    val sample = Array(111,222)

    @Test
    def should_handle_patch_before = {
      val patched = ResourceTraversable(resource).patch(2, sample, -1)
      assert (patched.isInstanceOf[InputResourceTraversable[_]], "new Traversable is not an InputResourceTraversable")
      assert (patched.size == 102)
      assert (patched.take(2).toArray == sample)
    }

    @Test
    def should_handle_append = {
      val patched = ResourceTraversable(resource) ++ sample
      assert (patched.isInstanceOf[InputResourceTraversable[_]], "new Traversable is not an InputResourceTraversable")
      assert (patched.size == 102)
      assert (patched.drop(100).toArray == sample)
    }

    @Test
    def should_handle_insert_with_patch = {
      val patched = ResourceTraversable(resource).patch(2, sample, 2)
      assert (patched.isInstanceOf[InputResourceTraversable[_]], "new Traversable is not an InputResourceTraversable")
      assert (patched.size == 102)
      assert (patched.drop(100).toArray == sample)
    }

    @Test
    def should_handle_map = {
      val strings = ResourceTraversable(resource) map {_.toString}
      assert (patched.isInstanceOf[InputResourceTraversable[_]], "new Traversable is not an InputResourceTraversable")
      assert (strings.head == "1")
    }
    
    @Test
    def should_handle_drop = {
      val dropped = ResourceTraversable(resource) drop(2)
      assert (dropped.isInstanceOf[InputResourceTraversable[_]], "new Traversable is not an InputResourceTraversable")
    }
    
    @Test
    def should_handle_drop_tomany = {
      val dropped = ResourceTraversable(resource) drop(10000)
      assert (dropped.isInstanceOf[InputResourceTraversable[_]], "new Traversable is not an InputResourceTraversable")
      assert (dropped.headOption.isEmpty)
    }
    
}