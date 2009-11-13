/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class PathSpec extends FileSystemFixture with Checkers with MustMatchers{

  def createContext = new Context(){
    def property (assertion: TestData => Unit) = {
      
    }
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
    (path checkAccess access) must be === (false)
    path createFile ()
    //        path.notExists must (beFalse and be_!=(path.exists))
    (path checkAccess access) must be === (true)
    path.delete()

    //        path.notExists must (beTrue and be_!=(path.exists))
    (path checkAccess access) must be === (false)
  }

  def respectsAccess(testData: TestData):Unit = {
    import testData._

    val path = Path(pathString)

  }
}
