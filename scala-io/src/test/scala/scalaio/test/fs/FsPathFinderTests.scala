/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import org.junit.Test
import org.junit.Assert._

abstract class FsPathFinderTests extends scalax.test.sugar.AssertionSugar with Fixture {
  def mkTree = {
    val root = fixture.path
    root / "a/b/c/d.scala" createFile ()
    root / "a/b/c/d2.css" createFile ()
    root
  }

  def assertSameContents[T](x1:TraversableOnce[T], x2:TraversableOnce[T]) = {
    val (list1,list2) = (x1 toList, x2 toList)

    assertEquals(list1.size, list2.size)
    assertTrue(list1.forall{list2 contains _})
  }
  @Test
  def `pathSet can be created with **` {
    val root = mkTree
    
    assertSameContents(root.descendants(), root ** "*")
    assertSameContents(List(root / "a/b/c/d.scala"), root ** "*.scala")
    assertSameContents(List(root / "a/b/c", root / "a/b"), root ** "c|b")
  }
}