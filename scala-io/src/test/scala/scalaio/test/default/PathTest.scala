/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.default

import scalax.io._
import Path.AccessModes._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import util.Random

import java.io.IOException

class PathTest extends scalax.test.sugar.AssertionSugar {
  implicit val codec = Codec.UTF8
  
  var fixture : FileSystemFixture = _
  
  @Before
  def before() : Unit = fixture = new DefaultFileSystemFixture(new TemporaryFolder())
  
  @After
  def after() : Unit = fixture.after()
  
  def fspath(name:String) = fixture.fs(name)
  def fspath(name:Path) = fixture.fs(name.path)
  
  @Test
  def path_should_support_standard_comparisons() : Unit = {
    check (false, standardPathComparisions _)
  }
  @Test
  def path_should_be_creatable_and_deletable() : Unit = {
    check (false, creatableAndDeletable _)
  }
  @Test @Ignore
  def path_should_respect_file_access_restrictions() : Unit = {
    check (false, respectsAccess _)
  }
  @Test 
  def path_should_have_exists_and_notExists_methods_that_are_not_equal() : Unit = {
    check (false, existsTest _)
  }
  @Test @Ignore
  def path_can_move_files() : Unit = {
    move( fixture.path.createFile (), fixture.path, fixture.path.createFile ())
  }
  @Test @Ignore
  def path_can_copy_files() : Unit = {
    copy( fixture.path.createFile (), fixture.path, fixture.path.createFile ())
  }
  @Test @Ignore
  def path_can_move_directories() : Unit = {
    move( fixture.path.createDirectory (), fixture.path, fixture.path.createDirectory ())
  }
  @Test @Ignore
  def path_can_copy_directories() : Unit = {
    copy( fixture.path.createDirectory (), fixture.path, fixture.path.createDirectory ())
  }
  @Test  @Ignore
  def path_can_move_directory_trees() : Unit = {
    move( fixture.tree(), fixture.path, fixture.tree(), canReplace=false)
  }
  @Test @Ignore
  def path_can_copy_directory_trees() : Unit = {
    copy( fixture.tree(), fixture.path, fixture.tree(), canReplace=false)
  }

  val check= fixture.check _

  def move(f1 :Path, f2: Path, exists: Path, canReplace: Boolean=true)={
    assertTrue(f1.exists)
    assertTrue(f2.notExists)
    f1 moveTo f2
    assertTrue(f2.exists)
    assertTrue(f1.notExists)

    f2 moveTo f2
    intercept[IOException] {
      f1 moveTo f2
    }
    intercept[IOException] {
      f2 moveTo exists
    }
    def replace = {
      f2.moveTo (exists, replaceExisting=true)
      assertTrue (f2.notExists)
      assertTrue (exists.exists)
    }
    if (canReplace) replace
    else intercept[IOException] {replace}
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
    path.access = access
    (Path.AccessModes.values -- access) foreach { a => matchAccess(a, path, false) }
    
    access foreach { a => matchAccess(a, path, true) }
  }
  def verifyAccess (test: => Unit)(is : Boolean)={
    if (is) test
    else intercept[IOException] {test}
  }

  def readTest(path: Path) = path.fileOps.chars().head

  def writeTest(path: Path) = path.fileOps.writeString("abc")

  def execTest(path: Path) = path.fileOps.execute()

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