/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scala.collection.immutable.Vector

import scalax.io._
import Path.AccessModes._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import util.Random

import java.io.IOException

trait AbstractDirectoryStreamTests extends scalax.test.sugar.AssertionSugar {
  /**
   * Node is a xml tree that represents the filesystem tree rooted at Path
   * The XML tree must have the same names as the filesystem tree.
   * It is essentially the "expected" tree
   */
  protected def fixtures(depth:Int=4) : (Path, Node)
  

  @Test //@Ignore
  def lists_entire_tree : Unit = {
    val (path,tree) = fixtures()

    val stream = path.descendants()
    assertSameStructure (stream, tree.children)
  }

  @Test //@Ignore
  def lists_entire_tree_with_depth_very_high : Unit = {
    val (path,tree) = fixtures()

    val stream = path.descendants(depth=Int.MaxValue)
    assertSameStructure (stream, tree.children)
  }

  @Test //@Ignore
  def lists_arbitrary_depth : Unit = {
    val (path,tree) = fixtures(4)
    
    val stream = path.descendants(depth=2)
    assertSameStructure (stream, tree.children, 2)
  }

  @Test //@Ignore
  def permits_filtering : Unit = {
    repeat {
      val (path,tree) = fixtures()
    
      val stream = path.descendants{_.name.length < 5}
      assertSameStructure (stream, tree.children){_.name.length < 5}
    }
  }

  @Test //@Ignore
  def exception_when_path_is_file : Unit = {
    val (path,tree) = fixtures()
    
    intercept[AssertionError] {
      (path \ "testfile").createFile().descendants()
    }
  }

  @Test //@Ignore
  def exception_when_next_called_on_empty_iterator : Unit = {
    val (path,tree) = fixtures()
    
    intercept[NoSuchElementException] {
      (path \ "testfile").createDirectory().descendants().iterator.next
    }
  }

  @Test //@Ignore
  def children_is_1_level_deep : Unit = {
    val (path,tree) = fixtures()
    
    val stream = path.children()
    
    assertTrue(stream forall {p => p.relativize(path).segments.size == 1})

    assertSameStructure (stream, tree.children, 1)
  }


  @Test //@Ignore
  def directory_stream_skips_read_protected_directories {
    val (root,tree) = fixtures()
    assertTrue(root.descendants() forall {_.exists})
    
    val totalFiles = root.descendants().size
    root.descendants().take(totalFiles/2) foreach {p => p.access = p.access - WRITE}
    
    assertEquals(totalFiles, root.descendants().size)
  }

  @Test //@Ignore
  def test_for_assertSameStructure {
      val (path,tree) = fixtures()
      
      intercept[AssertionError] {
        val stream = Nil
        assertSameStructure (stream, tree.children)
      }
  }

 def assertSameStructure(path : Iterable[Path], tree : Seq[Node], maxDepth : Int = Int.MaxValue)
                        (implicit filter : Node => Boolean = _ => true) {
   val paths = path.toList map {_.path}
   val pathsAsString = paths mkString "\n"
   var count = 0
   
   def walk(tree:Seq[Node], depth : Int) : Unit =  {
     if(depth <= maxDepth ) {
       tree.filter(filter) foreach { n =>
         count += 1
         assertTrue("expected "+n.path+" to be in "+pathsAsString, paths contains {n.path})
       }
       tree foreach {n=>walk(n.children, depth+1)}
     }
   }
   
   walk(tree,1)
   
   assertEquals(count, path.size)
 }
 
}