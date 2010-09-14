/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import Line.Terminators._

import scalaio.test._

abstract class FsInputTests extends AbstractInputTests with Fixture {
    protected def input(t:Type) = t match {
        case t @ TextNewLine => fixture.text(t.sep)
        case t @ TextPair => fixture.text(t.sep)
        case t @ TextCarriageReturn => fixture.text(t.sep)
        case TextCustom(sep) => fixture.text(sep)
        case Image => fixture.image
    }
}