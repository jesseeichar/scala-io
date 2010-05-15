/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.default

import scalax.io._
import scalax.io.ram._
import Path.AccessModes._

import org.junit.Assert._
import org.junit.{
  Test, Ignore
}
import util.Random
import scalaio.test.{
  TestData
}

import java.io.IOException

class PathTest extends scalax.test.sugar.AssertionSugar with DefaultFixture {
  implicit val codec = Codec.UTF8
  
  def fspath(name:String) = fixture.fs(name)
  def fspath(name:Path) = fixture.fs(name.path)

  @Test // @Ignore  
  def relativize_should_make_a_child_relative_to_parent = {
    val p = fixture.root \ "c1" \ "c2"
    assertEquals(2, (p relativize fixture.root).segments.size)
    assertFalse(fixture.root.path == p.path)
  }

  @Test //@Ignore  
  def relativize_return_other_when_not_same_fileSystem = {
    val other = new RamFileSystem()("other")
    assertSame(other, fixture.root relativize other)
  }
  @Test //@Ignore
  def createFile_should_fail_to_overwrite_exiting_by_default = {
    val p = (fixture.root \ "c1")
    
    p.createFile()
    
    intercept[java.io.IOException] {
      p.createFile()
    }
  }

  @Test //@Ignore
  def createFile_should_allow_option_to_fail_when_parent_is_missing = {
    val p = (fixture.root \ "c1" \ "c2" \ "c3")
    intercept[java.io.IOException] {
      p.createFile(createParents=false)
    }
    assertTrue(p.notExists)
  }
  @Test //@Ignore
  def createFile_should_create_parent_file_by_default = {
    val p = (fixture.root \ "c1" \ "c2" \ "c3").createFile()
    assertTrue(p.exists)
  }
  @Test //@Ignore  
  def slash_method_should_create_child_path = {
    val p = fixture.root \ "c1" \ "c2"
    assertEquals(2, (p relativize fixture.root).segments.size)
  }
  @Test //@Ignore
  def slash_method_should_split_on_separator = {
    val p = fixture.root \ ("c1"+fixture.fs.separator+"c2")
    assertEquals(2, (p relativize fixture.root).segments.size)
  }
  @Test //@Ignore
  def path_should_support_standard_comparisons() : Unit = {
    check (false, standardPathComparisions _)
  }
  @Test //@Ignore
  def path_should_be_creatable_and_deletable() : Unit = {
    check (false, creatableAndDeletable _)
  }
  @Test //@Ignore
  def path_should_respect_file_access_restrictions() : Unit = {
    check (false, respectsAccess _)
  }
  @Test //@Ignore
  def path_should_have_exists_and_notExists_methods_that_are_not_equal() : Unit = {
    check (false, existsTest _)
  }
  @Test //@Ignore
  def path_can_move_files() : Unit = {
    val f1 = fixture.path
    f1.ops.writeString("file to move")
    
    val exists = fixture.path
    exists.ops.writeString("pre existing file")
    
    move( f1, fixture.path, exists)
  }
  @Test //@Ignore
  def directories_cannot_overwrite_files = {
    val f = fixture
    import f.path
    intercept[IOException] {
      path.createDirectory() moveTo (path.createFile(), replace=true)
    }
  }
  @Test //@Ignore
  def files_cannot_overwrite_directories = {
    val f = fixture
    import f.path
    intercept[IOException] {
      path.createFile() moveTo (path.createDirectory(), replace=true)
    }
  }
  @Test @Ignore
  def path_can_move_directories() : Unit = {
    move( fixture.path.createDirectory (), fixture.path, fixture.path.createDirectory ())
  }
  @Test @Ignore
  def path_can_copy_files() : Unit = {
    copy( fixture.path.createFile (), fixture.path, fixture.path.createFile ())
  }
  @Test @Ignore
  def path_can_copy_directories() : Unit = {
    copy( fixture.path.createDirectory (), fixture.path, fixture.path.createDirectory ())
  }
  @Test  @Ignore
  def path_can_move_directory_trees() : Unit = {
    move( fixture.tree()._1, fixture.path, fixture.tree()._1, canReplace=false)
  }
  @Test @Ignore
  def path_can_copy_directory_trees() : Unit = {
    copy( fixture.tree()._1, fixture.path, fixture.tree()._1, canReplace=false)
  }

  def check = fixture.check _

  def move(f1 :Path, f2: Path, exists: Path, canReplace: Boolean=true)={
    assertTrue("expected 'exists' to exist before test", exists.exists)
    assertTrue("expected f1 to exist before move", f1.exists)
    assertTrue("expected f2 to NOT exist before move", f2.notExists)
    assertEquals(f2, f1 moveTo f2)
    assertTrue("expected f2 to exist after move", f2.exists)
    assertTrue("expected f1 to NOT exist after move", f1.notExists)

    f2 moveTo f2
    assertTrue("expected f2 to exist after move to self", f2.exists)
    intercept[IOException] {
      f1 moveTo f2
    }
    assertTrue("expected f2 to exist after attempting to move a nonexisting f1 to f2", f2.exists)
    
    val existsBeforeMove = if(exists.isFile) {
        val content = exists.ops.chars mkString ""
        assertFalse("contents of exists should not equal f2", content == (f2.ops.chars mkString ""))
        content
      } else {
        ""
      }
    intercept[IOException] {
      f2 moveTo exists
    }
    assertTrue("expected f2 to exist after attempting a non-overwrite move to an existing file", f2.exists)
    assertTrue("expected exists to exist after attempting a non-overwrite", exists.exists)
    if(exists.isFile) {
      assertTrue("expected exists to have the same contents after an illegal replace", existsBeforeMove == (exists.ops.chars mkString ""))
    }
    def tryReplace = {
      assertTrue("expected f2 to exist before replace", f2.exists)
      f2.moveTo (exists, replace=true)
      assertTrue (f2.notExists)
      assertTrue (exists.exists)
    }
    
    if (canReplace) tryReplace
    else intercept[IOException] {tryReplace}
  }


  def copy(f1 :Path, f2: Path, exists: Path, canReplace: Boolean=true)={
    assertTrue(f1.exists)
    assertTrue(f2.notExists)
    f1 copyTo f2
    assertTrue(f2.exists)
    assertTrue(f1.exists)

    f2 copyTo f2 // noop
    intercept[IOException] {
      f1 copyTo f2
    }
    intercept[IOException] {
      f2 copyTo exists
    }
    def overwrite = {
      f2.copyTo (exists, replaceExisting=true)
      assertTrue(f2.exists)
      assertTrue(exists.exists)
    }
    if (canReplace) overwrite
    else intercept[IOException] {overwrite}
  }
  
  def standardPathComparisions(testData: TestData):Unit = {
    import testData._

    val path = fspath(pathName)
    assertEquals(pathName, path.path)
    assertTrue(pathName endsWith path.name)

    assertTrue(path endsWith fspath(path.name))
    assertTrue(path startsWith fspath(path.segments.head))
    assertTrue(path isSame fspath(pathName))
    assertEquals(fspath(pathName), path)
    assertEquals(path, fspath(pathName))
    assertEquals(path.hashCode, fspath(pathName).hashCode)


    val pathx = path.resolve(fspath("x"))
    
    assertNotNull(pathx)
    assertEquals(pathx, path.resolve("x"))
    assertEquals("x", pathx.segments.last)
    assertEquals(fspath("x"), pathx.relativize(path))
  }

  def creatableAndDeletable(testData: TestData):Unit = {
    import testData._

    val path = fspath(pathName)

    assertTrue(path.notExists)
    assertFalse(path.exists)
    assertFalse("expected path access to NOT be "+access, !access.isEmpty && (path checkAccess (access:_*)))
    path.createFile (accessModes = access)

    assertFalse(path.notExists)
    assertTrue(path.exists)
    assertTrue("expected path access to be "+access+".  Access is "+path.access, path checkAccess (access:_*))
    path.access = List(WRITE)
    path.delete()

    assertTrue(path.notExists)
    assertFalse(path.exists)
    assertFalse(!access.isEmpty && (path checkAccess (access:_*)))
  }

  def existsTest(testData : TestData) : Unit = {
      val path = fspath(testData.pathName)
      assertTrue(path.exists != path.notExists)
      assertFalse(path.exists)
      path.createFile()
      assertTrue(path.exists)
      assertTrue(path.exists != path.notExists)
  }

  def respectsAccess(testData: TestData):Unit = {
    import testData._

    val path = fspath(pathName)
    path.createFile()
    path.ops.writeString("some test data")
    path.access = access
    (Path.AccessModes.values -- access) foreach { a => matchAccess(a, path, false) }
    
    access foreach { a => matchAccess(a, path, true) }
  }
  def verifyAccess (test: => Unit)(is : Boolean)={
    if (is) test
    else intercept[IOException] {test}
  }

  def readTest(path: Path) = path.ops.chars.head

  def writeTest(path: Path) = path.ops.writeString("abc")

  def execTest(path: Path) = path.ops.execute()

  def matchAccess(access: AccessMode, path: Path, is: Boolean) = access match {
    case EXECUTE => verifyAccess (execTest(path))(is)
    case WRITE => verifyAccess (writeTest(path))(is)
    case READ => verifyAccess (readTest(path))(is)
  }
  /*  
    AccessModes.values intersect (access) foreach { a => verifyTest(access, path, is) }

    access foreach { a => verifyTest(access, path, is) }
  */
}