/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.stream

import scalax.io.resource._
import scalax.io.Codec
import scalaio.test._

class ReadCharsTest extends AbstractReadCharsTests {
  implicit val c = Codec.UTF8

  private def text(sep: String) = {
    val string = Constants.TEXT_VALUE.replaceAll("""\n""", sep)
    new java.io.StringReader(string)
  }

  protected def readChars(t: Type) = t match {
    case t@TextNewLine => Resource.fromReader(text(t.sep))
    case t@TextPair => Resource.fromReader(text(t.sep))
    case t@TextCarriageReturn => Resource.fromReader(text(t.sep))
    case TextCustom(sep) => Resource.fromReader(text(sep))
  }

  override protected def sizeIsDefined = false
}
