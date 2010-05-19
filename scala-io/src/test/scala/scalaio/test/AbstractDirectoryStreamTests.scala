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
  
    
  @Test
  def lists_entire_tree : Unit = {
    val (path,tree) = fixtures()

    val stream = path.descendants()
    assertSameStructure (stream, tree.children)
  }
  
  @Test
  def lists_arbitrary_depth : Unit = {
    val (path,tree) = fixtures()
    
    val stream = path.descendants(depth=2)
    assertSameStructure (stream, tree.children, 2)
  }

  @Test
  def permits_filtering : Unit = {
    val (path,tree) = fixtures()
    
    val stream = path.descendants{_.name.length < 5}
    assertSameStructure (stream, tree.children){_.name.length < 5}
  }

  @Test
  def exception_when_path_is_file : Unit = {
    val (path,tree) = fixtures()
    
    intercept[AssertionError] {
      (path \ "testfile").createFile().descendants()
    }
  }

  @Test
  def exception_when_next_called_on_empty_iterator : Unit = {
    val (path,tree) = fixtures()
    
    intercept[NoSuchElementException] {
      (path \ "testfile").createDirectory().descendants().iterator.next
    }
  }

  @Test
  def children_is_1_level_deep : Unit = {
    val (path,tree) = fixtures()
    
    val stream = path.children()
    assertSameStructure (stream, tree.children, 1)
    
    assertTrue(stream forall {p => p.relativize(path).segments.size == 1})
  }

 def assertSameStructure(path : Iterable[Path], tree : Seq[Node], maxDepth : Int = Int.MaxValue)
                        (implicit filter : Node => Boolean = _ => true) {
   def walk(tree:Seq[Node], depth : Int) = {
     if(depth <= maxDepth ) {
       tree.filter(filter) foreach { n =>
         assertTrue(path exists {_.path == n.path})
       }
     }
   }
 }
 
 
}