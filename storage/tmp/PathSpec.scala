/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import Path.AccessModes._

import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import java.io.IOException

@RunWith(classOf[JUnitRunner])
class PathSpec extends FileSystemFixture with Checkers with MustMatchers {


  def createContext = new Context(){
    def property (assertion: TestData => Unit):Prop = {
      null
    }
    def testData = new TestData("/tmp/file", 2)
  }

  "A Path" should {
    "support standard comparisons" in { fixture: Context =>
      check (fixture.property (standardPathComparisions _))
    }

    "be creatable and deletable" in { fixture: Context =>
      check (fixture.property (creatableAndDeletable _))
    }

    "Respect file access restrictions" in {  fixture: Context =>
      check (fixture.property (respectsAccess _))
    }

    "have exists and notExists methods that are not be equal" in { fixture: Context =>
      check (fixture.property (existsTest _))
    }
    
    
    "Path" can {
    "move files" in {
    move( context.file, context.path, context.file)
    }
    "copy files" in {
    copy( context.file, context.path, context.file)
    }
    "move directories" in {
    move( context.dir, context.path, context.dir)
    }
    "copy directories" in {
    copy( context.dir, context.path, context.dir)
    "move directory trees" in {
    move( context.tree, context.path, context.tree, canReplace=false)
    }
    "copy directory trees" in {
    copy( context.tree, context.path, context.tree, canReplace=false)
    }
    }
  }

  def verifyAccess (test: => Unit)(is : Boolean)={
  if (is) test
  else intercept[IOException] {test}
  }

  def readTest(path: Path) = path.fileOps.chars(0)

  def writeTest(path: Path) = path.fileOps.write("abc")

  def execTest(path: Path) = path.execute()

  def matchAccess(access: AccessMode, path: Path, is: Boolean) = access match {
  case EXECUTE => verifyAccess (execTest(path))(is)
  case WRITE => verifyAccess (writeTest(path))(is)
  case READ => verifyAccess (readTest(path))(is)
  }
  }
  AccessModes.values intersect (access) foreach { a => verifyTest(access, path, is) }

  access foreach { a => verifyTest(access, path, is) }

  def move(f1 :Path, f2: Path, exists: Path, canReplace: Boolean=true)={
  f1 must ('exist)
  f2 must ('notExists)
  f1 moveTo f2
  f2 must ('exists)
  f1 must ('notExists)

  f2 moveTo f2
  intercept[IOException] {
  f1 moveTo f2
  }
  intercept[IOException] {
  f2 moveTo exists
  }
  def replace = {
  f2 moveTo (exists, replaceExisting=true)
  f2 must ('notExists)
  exists must ('exists)
  }
  if (canReplace) replace
  else intercept[IOException] {replace}
  }


  def copy(f1 :Path, f2: Path, exists: Path, canReplace: Boolean=true)={
  f1 must ('exist)
  f2 must ('notExists)
  f1 copyTo f2
  f2 must ('exists)
  f1 must ('exists)

  f2 copyTo f2 // noop
  intercept[IOException] {
  f1 copyTo f2
  }
  intercept[IOException] {
  f2 copyTo exists
  }
  def overwrite = {
  f2 copyTo (exists, replaceExisting=true)
  f2 must ('exists)
  exists must ('exists)
  }
  if (canReplace) overwrite
  else intercept[IOException] {overwrite}

  }
  def standardPathComparisions(testData: TestData):Unit = {
    import testData._

    val path = Path(pathString)
    path.path must be === (pathString)
    pathString must endWith (path.name)

    path.endsWith(Path(path.name)) must be === (true)
    path.startsWith(Path(path.segments.head)) must be === (true)
    path.isSame(Path(pathString)) must be === (true)
    path must be === (Path(pathString))
    Path(pathString) must be === (path)
    path.hashCode must be === (Path(pathString).hashCode)

    val pathx = path.resolve(Path("x"))
    pathx must be === (path.resolve("x"))
    pathx.segments.head must be === ("x")
    pathx.relativize(path) must be === (path)
  }

  def creatableAndDeletable(testData: TestData):Unit = {
    import testData._

    val path = Path(pathString)

    //        path.notExists must (beTrue and be_!=(path.exists))
    (path checkAccess (access:_*)) must be === (false)
    path createFile ()
    //        path.notExists must (beFalse and be_!=(path.exists))
    (path checkAccess (access:_*)) must be === (true)
    path.delete()

    //        path.notExists must (beTrue and be_!=(path.exists))
    (path checkAccess (access:_*)) must be === (false)
  }

  def respectsAccess(testData: TestData):Unit = {
    import testData._

    val path = Path(pathString)
    (Path.AccessModes.values -- access) foreach { a => matchAccess(a, path, false) }
    
    access foreach { a => matchAccess(a, path, true) }
  }

  def existsTest(testData : TestData) : Unit = {
      val path = Path(testData.pathString)
      path.exists must not be (path.notExists)
      path must not be ('exists)
      path.createFile()
      path must be ('exists)
      path.exists must not be ('notExists)
  }

  def verifyAccess (test: => Unit)(is : Boolean)={
    if (is) test
    else intercept[IOException] {test}
  }
  
  def readTest(path: Path) = path.fileOps.chars().head
  
  def writeTest(path: Path) = path.fileOps.writeString("abc")
  
  def execTest(path: Path) = path.execute()
  
  def matchAccess(access: AccessMode, path: Path, is: Boolean) = access match {
    case EXECUTE => verifyAccess (execTest(path))(is)
    case WRITE => verifyAccess (writeTest(path))(is)
    case READ => verifyAccess (readTest(path))(is)
  }

}
