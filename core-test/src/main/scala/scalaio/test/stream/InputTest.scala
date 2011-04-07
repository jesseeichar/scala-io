/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.stream

import scalaio.test._
import java.io.ByteArrayInputStream
import scalax.io.{Codec, Resource}
import org.junit.Test
import org.junit.Assert._

class InputTest extends AbstractInputTests {
  private def text(sep: String) = {
    val bytes = Constants.TEXT_VALUE.replaceAll("""\n""", sep).getBytes(Codec.UTF8.name)
    new java.io.ByteArrayInputStream(bytes)
  }

  protected def input(t: Type) = t match {
    case t@TextNewLine => Resource.fromInputStream(text(t.sep))
    case t@TextPair => Resource.fromInputStream(text(t.sep))
    case t@TextCarriageReturn => Resource.fromInputStream(text(t.sep))
    case TextCustom(sep) => Resource.fromInputStream(text(sep))
    case TextCustomData(sep, data) => Resource.fromInputStream(
      new ByteArrayInputStream(data.getBytes(Codec.UTF8.charSet))
    )
    case Image => Resource.fromInputStream(Constants.IMAGE.openStream())
  }

  override protected def sizeIsDefined = false

  @Test
  def issue_8_lines_in_Input_not_lazy {
    import scalax.io.Line.Terminators._

    val file = largeResource(Key.TEXT)
    
    val start = System.currentTimeMillis
    val fromFile = Resource.fromFile(file).lines(NewLine)(codec=Codec.UTF8)
    val fromString = Resource.fromFile(file.getAbsolutePath).lines(NewLine)(codec=Codec.UTF8)
    fromString.toString
    fromFile.toString
    val end = System.currentTimeMillis
    assertTrue(end-start < 500)
  }

}

