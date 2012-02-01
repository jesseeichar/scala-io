/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import scalax.test.sugar._
import org.junit.Assert._
import org.junit.Test
import java.io.InputStream

class CloseActionTest extends AssertionSugar with IOSugar {
  implicit val codec = Codec.UTF8

  val source = "sample"

  def resource(closeAction:CloseAction[InputStream]) = new InputStreamResource(source.inputStream, closeAction, () => None, UnknownName())

  @Test
  def close_actions_are_executed_at_resource_close = {
    var c = 0
    val resource2 = resource(CloseAction((_:Any) => c += 1))

    assertEquals(source, resource2.slurpString)
    assertEquals(1, c)
    resource2.bytes.force
    assertEquals(2, c)
    resource2.chars.force
    assertEquals(3, c)
    resource2.readableByteChannel.bytes.force
    assertEquals(4, c)
  }

  @Test
  def close_actions_can_be_combined = {
    var c = 0
    var d = 0
    val closer = CloseAction((_:Any) => c += 1) :+ CloseAction((_:Any) => {assertEquals(1,c);d += 1})
    val resource2 = resource(closer)

    assertEquals(source, resource2.slurpString)
    assertEquals(1, c)
    assertEquals(1, d)
  }


}
