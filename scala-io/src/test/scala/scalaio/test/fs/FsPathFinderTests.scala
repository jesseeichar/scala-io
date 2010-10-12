/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import org.junit.Assert._
import scalax.io.Path
import scalax.io.PathMatcher._

import org.junit.{Ignore, Test}

abstract class FsPathFinderTests extends scalax.test.sugar.AssertionSugar with Fixture {
  def mkTree = {
    val root = fixture.root
    root / "a/b/c/d.scala" createFile ()
    root / "a/b/c/d2.css" createFile ()
    root / "z/y/x/w.html" createFile ()
    root
  }

  def assertSameContents(x1:TraversableOnce[Path], x2:TraversableOnce[Path]) = {
    val (list1,list2) = (x1 toList, x2 toList)

    def relativize(l:List[Path]) = l.map{_.relativize(fixture.root)}

    assertEquals("Expected "+relativize(list1)+" but got "+relativize(list2),list1.size, list2.size)
    assertTrue("Expected "+relativize(list1)+" but got "+relativize(list2), list1.forall{list2 contains _});
  }
  @Test //@Ignore
  def `pathFinder can be created with **` {
    val root = mkTree

    assertSameContents(root.descendants(), root ** "*")
    assertSameContents(List(root / "a/b/c/d.scala"), root ** "*.scala")
    assertSameContents(List(root / "a/b/c", root / "a/b"), root ** "{c,b}")
  }
  @Test //@Ignore
  def `pathFinder can be created with *` {
    val root = mkTree

    assertSameContents(root.children(), root * "*")
    assertSameContents(Nil, root * "*.scala")
    assertSameContents(List(root / "a"), root * "{a,b}")
  }
  @Test //@Ignore
  def `*** is an alias for descendants` {
    val root = mkTree
    assertSameContents(root.descendants(), root ***)
  }
  @Test //@Ignore
  def `pathfinders can be expanded on with **` {
    val root = mkTree

    assertSameContents(root ** "*.scala" toList, root * "a" ** "*.scala")
    assertSameContents(root / "a" ** "*.css" toList, root * "a" ** "*.css")

    // check that multiple deep calls works
    assertSameContents(root / "a" ** "*.css" toList, root * "a" ** "b" ** "*.css")
    assertSameContents((root / "a/b" ***) toList, root * "a" ** "b" ***)
    assertSameContents(List(root / "a/b/c"), root * "a" ** "b" * "c")
    assertSameContents(List(root / "a/b/c" ), root * "a" ** "b" / "c")
    assertSameContents(Nil, root * "a" ** "b" / "*")
    // TODO asBase

  }
  @Test //@Ignore
  def `pathfinders can be expanded on with *` {
    val root = mkTree

    assertSameContents(List(root / "a/b"), root * "a" * "*")
    assertSameContents(List(root / "a/b", root / "z/y/x/w.html"), root ** "{a,x}" * "*")
    assertSameContents(List(root / "a/b", root / "z/y"), root * "*" * "*")


    // check that multiple deep calls works
    assertSameContents(root / "a" ** "*.css" toList, root * "a" * "b" ** "*.css")
    assertSameContents((root / "a/b" ***) toList, root * "a" * "b" ***)
    assertSameContents(List(root / "a/b/c"), root * "a" * "b" * "c")
    assertSameContents(List(root / "a/b/c" ), root * "a" * "b" / "c")
    assertSameContents(Nil, root * "a" * "b" / "*")
    // TODO asBase
  }
  @Test //@Ignore
  def `pathfinders can be expanded on with ***` {
    val root = mkTree

    assertSameContents((root / "a" ***) toList, root * "a" ***)
  }
  @Test //@Ignore
  def `pathfinders can be expanded on with /` {
    val root = mkTree

    assertSameContents(List(root / "a/b"), root * "a" / "b")
    assertSameContents(List(root / "a/b"), root * "*" / "b")
  }
  @Test //@Ignore
  def `pathfinders can be combined using +++` {
    val root = mkTree

    assertSameContents(List(root ** "*.scala", root * "a").flatten, (root * "a") +++ (root ** "*.scala"))
    assertSameContents(List(root / "a", root / "z"), (root / "a") +++ (root / "z"))

    assertSameContents(List(root / "a", root / "z"), (root / "a") +++ (root / "z"))

    // check that multiple deep calls works
    assertSameContents(root / "a" ** "*.css" toList, ((root * "a") +++ (root * "z")) ** "*.css")
    assertSameContents((root * "{a,z}" ***) toList, ((root * "a") +++ (root * "z")) ***)
    assertSameContents(List(root / "a/b/c"), ((root * "a") +++ (root * "z")) * "b" * "c")
    assertSameContents(List(root / "a/b" ), ((root * "a") +++ (root * "z")) / "b")
    assertSameContents(List(root / "a/b" ), ((root * "a") +++ (root * "z")) \ "b")
    assertSameContents(Nil, ((root * "a") +++ (root * "z")) / "*")
    // TODO asBase

    // TODO add some from RamFS
  }
  @Test //@Ignore
  def `pathfinders can be combined using ---` {
    val root = mkTree
    import scalax.io.PathMatcher.NameIs
    assertSameContents(root.descendants{- NameIs("d2.css")} toList, (root ***) --- (root ** "*.css"))
    assertSameContents(Nil, (root / "a") --- (root / "a"))

    // check that multiple deep calls works
    assertSameContents(root / "a" ** "*.css" toList, ((root * "*") --- (root * "z")) ** "*.css")
    assertSameContents((root * "a" ***) toList, ((root * "*") --- (root * "z")) ***)
    assertSameContents(List(root / "a/b/c"), ((root * "*") --- (root * "z")) * "b" * "c")
    assertSameContents(List(root / "a/b" ), ((root * "*") --- (root * "z")) / "b")
    assertSameContents(List(root / "a/b" ), ((root * "*") --- (root * "z")) \ "b")
    assertSameContents(Nil, ((root * "*") --- (root * "z")) / "*")
    // TODO asBase

    // TODO remove some RamFs paths
  }
  @Test //@Ignore
  def`asBase allows relative copying of path sets`{
    fail("not implemented")
  }
}