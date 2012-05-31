/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.channel

import scalaio.test._
import scalax.io._
import java.nio.channels._
import java.io._

class OutputTest extends AbstractOutputTests[ReadableByteChannel, WritableByteChannel] {
  def open(closeAction:CloseAction[WritableByteChannel] = CloseAction.Noop) = {
    val oStream = new ByteArrayOutputStream()
    val out = Channels.newChannel(oStream)
    def in = Channels.newChannel(new ByteArrayInputStream(oStream.toByteArray))

    val inResource = Resource.fromReadableByteChannel(in)
    val outResource = Resource.fromWritableByteChannel(out).addCloseAction(closeAction)

    (inResource, outResource)
  }
  def errorOnWriteOut = Resource.fromWritableByteChannel(Channels.newChannel(errorStream))
  override def writeErrorRaisedOnClose = true
}
