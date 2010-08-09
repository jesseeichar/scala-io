/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.ramfs

import scalax.io._
import Line.Terminators._

import scalaio.test._

class RamFileInputTest extends AbstractInputTests with RamFixture {
    protected def input(t:Type) = t match {
        case t @ TextNewLine => fixture.text(t.sep).ops
        case t @ TextPair => fixture.text(t.sep).ops
        case t @ TextCarriageReturn => fixture.text(t.sep).ops
        case TextCustom(sep) => fixture.text(sep).ops
        case Image => fixture.image.ops
    }
}