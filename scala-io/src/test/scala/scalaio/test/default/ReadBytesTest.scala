/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.default

import scalax.io._
import org.junit.{
  Before, After
}
import org.junit.rules.TemporaryFolder
import scalaio.test._

class ReadBytesTest extends AbstractReadBytesT {

    var fixture : FileSystemFixture = _

    @Before def before() : Unit = fixture = new DefaultFileSystemFixture(new TemporaryFolder())

    @After def after() : Unit = fixture.after()

    protected def readBytes(t:Type) = t match {
      case Text => fixture.text.fileOps
      case Image => fixture.image.fileOps
    }
}