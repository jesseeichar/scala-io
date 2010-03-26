/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.stream

import scalax.io.resource._
import scalaio.test._

class InputTest extends AbstractInputTests {
    protected def input(t:Type) = t match {
      case Text => Resource.fromInputStream(Constants.TEXT.openStream())
      case Image => Resource.fromInputStream(Constants.IMAGE.openStream())
    }

    override protected def sizeIsDefined = false
}
