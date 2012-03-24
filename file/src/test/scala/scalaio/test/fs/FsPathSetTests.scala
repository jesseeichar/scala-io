/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import org.junit.Assert._
import scalax.file.Path
import org.junit.Test
import scalax.file.ramfs.RamFileSystem
import scalaio.test.Node
import scalaio.test.AbstractPathSetTests
import scalax.file.PathSet

abstract class FsPathSetTests extends scalax.test.sugar.AssertionSugar with AbstractPathSetTests with Fixture {
    protected def fixtures(depth:Int=4) : (Path, Node) = fixture.tree(depth)

  def mkTree = {
    val root = fixture.root
    root / "a"/"b"/"c"/"d.scala" createFile ()
    root / "a"/"b"/"c"/"d2.css" createFile ()
    root / "z"/"y"/"x"/"w.html" createFile ()
    root
  }

  def assertSameContents(x1:TraversableOnce[Path], x2:TraversableOnce[Path]) = {
    val list1 = x1 toList
    val list2 = x2 toList

    def relativize(l:List[Path]) = l.map{_.relativize(fixture.root)}

    assertEquals("Expected "+relativize(list1)+" but got "+relativize(list2),list1.size, list2.size)
    assertTrue("Expected "+relativize(list1)+" but got "+relativize(list2), list1.forall{list2 contains _});
  }
  @Test //@Ignore
  def pathset_can_be_created_with_star_star {
    val root = mkTree

    assertSameContents(root.descendants(), root ** "*")
    assertSameContents(List(root / "a"/"b"/"c"/"d.scala"), root ** "*.scala")
    assertSameContents(List(root / "a"/"b"/"c", root / "a"/"b"), root ** "{c,b}")
  }
  @Test //@Ignore
  def `pathset can be created with *` {
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
  def `pathsets can be expanded on with **` {
    val root = mkTree

    assertSameContents(root ** "*.scala" toList, root * "a" ** "*.scala")
    assertSameContents(root / "a" ** "*.css" toList, root * "a" ** "*.css")

    // check that multiple deep calls works
    assertSameContents(root / "a" ** "*.css" toList, root * "a" ** "b" ** "*.css")
    assertSameContents((root / "a"/"b" ***) toList, root * "a" ** "b" ***)
    assertSameContents(List(root / "a"/"b"/"c"), root * "a" ** "b" * "c")
    assertSameContents(List(root / "a"/"b"/"c" ), root * "a" ** "b" / "c")
    assertSameContents(Nil, root * "a" ** "b" / "*")
    // TODO asBase

  }
  @Test //@Ignore
  def `pathsets can be expanded on with *` {
    val root = mkTree

    assertSameContents(List(root / "a"/"b"), root * "a" * "*")
    assertSameContents(List(root / "a"/"b", root / "z"/"y"/"x"/"w.html"), root ** "{a,x}" * "*")
    assertSameContents(List(root / "a"/"b", root / "z"/"y"), root * "*" * "*")


    // check that multiple deep calls works
    assertSameContents(root / "a" ** "*.css" toList, root * "a" * "b" ** "*.css")
    assertSameContents((root / "a"/"b" ***) toList, root * "a" * "b" ***)
    assertSameContents(List(root / "a"/"b"/"c"), root * "a" * "b" * "c")
    assertSameContents(List(root / "a"/"b"/"c" ), root * "a" * "b" / "c")
    assertSameContents(Nil, root * "a" * "b" / "*")
    // TODO asBase
  }
  @Test //@Ignore
  def `pathsets can be expanded on with ***` {
    val root = mkTree

    assertSameContents((root / "a" ***) toList, root * "a" ***)
  }
  @Test //@Ignore
  def `pathsets can be expanded on with /` {
    val root = mkTree

    assertSameContents(List(root / "a"/"b"), root * "a" / "b")
    assertSameContents(List(root / "a"/"b"), root * "*" / "b")
  }
  @Test //@Ignore
  def `pathsets can be combined using +++` {
    val root = mkTree

    assertSameContents(List(root ** "*.scala", root * "a").flatten, (root * "a") +++ (root ** "*.scala"))
    assertSameContents(List(root / "a", root / "z"), (root / "a") +++ (root / "z"))

    assertSameContents(List(root / "a", root / "z"), (root / "a") +++ (root / "z"))

    // check that multiple deep calls works
    assertSameContents(root / "a" ** "*.css" toList, ((root * "a") +++ (root * "z")) ** "*.css")
    assertSameContents((root * "{a,z}" ***) toList, ((root * "a") +++ (root * "z")) ***)
    assertSameContents(List(root / "a"/"b"/"c"), ((root * "a") +++ (root * "z")) * "b" * "c")
    assertSameContents(List(root / "a" / "b" ), ((root * "a") +++ (root * "z")) / "b")
    assertSameContents(List(root / "a"/"b" ), ((root * "a") +++ (root * "z")) \ "b")
    assertSameContents(Nil, ((root * "a") +++ (root * "z")) / "*")
    // TODO asBase

    val ramfs = new RamFileSystem()
    ramfs.root / "g"/"h"/"i.xml" createFile()
    assertSameContents(List(root / "a"/"b"/"c"/"d.scala", ramfs.root / "g"/"h"/"i.xml"), (root ** "*.scala") +++ (ramfs.root ** "*.xml"))
  }
  @Test //@Ignore
  def `pathsets can be combined using ---` {
    val root = mkTree
    import scalax.file.PathMatcher.NameIs
    
    assertSameContents(root.descendants{- NameIs("d2.css")} toList, (root ***) --- (root ** "*.css"))
    assertSameContents(Nil, (root / "a") --- (root / "a"))

    // check that multiple deep calls works
    val d = ((root * "*") --- (root * "z"))
    d  ** "*.css"
    assertSameContents(root / "a" ** "*.css" toList, ((root * "*") --- (root * "z")) ** "*.css")
    assertSameContents((root * "a" ***) toList, ((root * "*") --- (root * "z")) ***)
    assertSameContents(List(root / "a"/"b"/"c"), ((root * "*") --- (root * "z")) * "b" * "c")
    assertSameContents(List(root / "a"/"b" ), ((root * "*") --- (root * "z")) / "b")
    assertSameContents(List(root / "a"/"b" ), ((root * "*") --- (root * "z")) \ "b")
    assertSameContents(Nil, ((root * "*") --- (root * "z")) / "*")
    // TODO asBase

    val ramfs = new RamFileSystem()
    ramfs.root / "a"/"b"/"c"/"d.scala" createFile()
    assertSameContents(List(root / "a"/"b"/"c"/"d.scala"), (root ** "*.scala") --- (ramfs.root ***))

  }

  @Test
  def pathsets_are_lazily_processed_for_map {

    val root = mkTree
    var i = 0
    val search = root * "*" map{p => i += 1; p}
    assertEquals(0,i)
    val filtered = search.filter(_.name.size < 5)
	  assertEquals(0,i)
    val flatMapped = search.flatMap(_.name)
    assertEquals(0,i)
    import scalax.file.ImplicitConversions.string2path
    val collected = search.collect{ case p if p.startsWith("p") => p } 
    assertEquals(0,i)
    
    val size = search.force.size
    assertEquals(size,i)
    
    i = 0
    filtered.size
    assertEquals(size,i)
    
    i = 0
    flatMapped.size
    assertEquals(size,i)

    i = 0
    collected.size
    assertEquals(size,i)
  }
  
  @Test //@Ignore
  def a_PathSet_can_be_made_from_a_collection_of_paths {
    val root = mkTree
    val set = PathSet(root / "a",root / "a" / "b", root / "z")
    
    val andChildren = set * "*" toList
    
    val pathJoin = (root / "a") +++ (root / "a" / "b") +++ (root / "z")
    val pathJoinChildren = (pathJoin * "*").toList
    
    assertEquals(pathJoinChildren.size, andChildren.size)
    pathJoinChildren.foreach(p => assertTrue(andChildren contains p))
    
    assertEquals(3, andChildren.size)
    assertTrue(andChildren contains (root / "a" / "b"))
    assertTrue(andChildren contains (root / "a" / "b" / "c"))
    assertTrue(andChildren contains (root / "z" / "y"))
  }
  
}
