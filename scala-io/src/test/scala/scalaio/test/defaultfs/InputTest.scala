/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.defaultfs

import scalax.io._
import Line.Terminators._

import scalaio.test._

class InputTest extends AbstractInputTests with DefaultFixture {
    protected def input(t:Type) = t match {
        case t @ TextNewLine => fixture.text(t.sep).ops
        case t @ TextPair => fixture.text(t.sep).ops
        case t @ TextCarriageReturn => fixture.text(t.sep).ops
        case TextCustom(sep) => fixture.text(sep).ops
        case Image => fixture.image.ops
    }
}