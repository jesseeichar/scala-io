/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.file._
import AccessModes._
import PathMatcher.GlobPathMatcher

import org.junit.Assert._
import org.junit.Test


trait AbstractPathSetTests extends scalax.test.sugar.FSAssertionSugar {
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
      val (path,tree) = fixtures()

      val stream = path.descendants{(_:Path).name.length < 5}
      assertSameStructure (stream, tree.children){_.name.length < 5}

      val name = tree.children.head.name
      val matcher = GlobPathMatcher("**/"+name)
      assertEquals(1,path.descendants(matcher).size)
      assertEquals(path.descendants().size - 1,path.descendants(- matcher).size)
  }

  @Test //@Ignore
  def exception_when_next_called_on_empty_iterator : Unit = {
    val (path,tree) = fixtures()

    intercept[NoSuchElementException] {
      (path \ "testfile").createDirectory().descendants().head
    }
  }

  @Test //@Ignore
  def children_is_1_level_deep : Unit = {
    val (path,tree) = fixtures()

    val stream = path.children()

    assertTrue(stream forall {p => path.relativize(p).segments.size == 1})

    assertSameStructure (stream, tree.children, 1)
  }

  @Test //@Ignore
  def path_set_skips_read_protected_directories {
    val (root,tree) = fixtures()
    assertTrue(root.descendants() forall {_.exists})

    val totalFiles = root.descendants().size
    root.descendants().take(totalFiles/2) foreach {p => p.access = p.access - Write}

    assertEquals(totalFiles, root.descendants().size)
  }


  @Test //@Ignore
  def path_set_toString_should_not_trigger_directory_traversal {
    val (root,tree) = fixtures()
    val toString = root.***.toString
    assertFalse(toString contains tree.head.name)
  }

  @Test //@Ignore
  def test_for_assertSameStructure {
      val (_,tree) = fixtures()

      intercept[AssertionError] {
        val stream = Nil
        assertSameStructure (stream, tree.children)
      }
  }

}
