/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scalaio.test

import scalax.io._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import ScalaIoMocks.fileSystemMock

@RunWith(classOf[JUnitRunner])
class PathObjectSpec extends WordSpec with MustMatchers {
  "Path object" should {
    "implicitly create path from string" in{
      import Path.string2path

      "nonsense path".fileSystem must be === FileSystem.default

      implicit val fs = fileSystemMock
      println(fs)
      val p:Path = "hi"
      println(p)
      "path".fileSystem must be === fs
    }
    "implicitly create path from a java file" in{
      import java.io.File
      import Path._

      new File("nonsense path").fileSystem must be === FileSystem.default

      implicit val fs = fileSystemMock
      new File("path").fileSystem must be === fs
    }
    "create paths from a string" in {
      Path("nonsense path").fileSystem must be === FileSystem.default

      implicit val fs = fileSystemMock
      Path("path").fileSystem must be === fs
    }
  }
}

