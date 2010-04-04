/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.default

import scalax.io._
import Line.Terminators._

import org.junit.{
  Before, After
}
import org.junit.rules.TemporaryFolder
import scalaio.test._

class InputTest extends AbstractInputTests {
    var fixture : FileSystemFixture = _

    @Before def before() : Unit = fixture = new DefaultFileSystemFixture(new TemporaryFolder())

    @After def after() : Unit = fixture.after()

    protected def input(t:Type) = t match {
        case t @ TextNewLine => fixture.text(t.sep).ops
        case t @ TextPair => fixture.text(t.sep).ops
        case t @ TextCarriageReturn => fixture.text(t.sep).ops
        case TextCustom(sep) => fixture.text(sep).ops
        case Image => fixture.image.ops
    }
}