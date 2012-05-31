/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import org.junit.Assert._
import org.junit.{
  Test,
  Ignore
}
import Constants.TEXT_VALUE
import java.io.ByteArrayInputStream
import java.io.Closeable
import java.nio.channels.Channels
import scala.collection.mutable.ArrayBuffer
import java.io.ByteArrayOutputStream
import java.io.FilterOutputStream
import java.io.InputStreamReader
import java.io.OutputStreamWriter

class UnmanagedResourceTests extends scalax.test.sugar.AssertionSugar {
  private final val DEFAULT_DATA = "default data"
  implicit val codec = Codec.UTF8

  class InputContext() {
    var closes = 0
    var creations = 0

    def in = new ByteArrayInputStream((1 to 100).mkString.getBytes("UTF8")) {
      creations += 1
      override def close() =
        closes += 1
    }
    val seekabledata = ArrayBuffer((1 to 100).mkString.getBytes("UTF8"): _*)
    def seekable(openOptions: OpenOption*) = {
      creations += 1
      new ArrayBufferSeekableChannel(seekabledata, openOptions: _*)(closeAction = _ =>
        closes += 1)
    }
    def assertRead[R, U](unmanaged: R)(expectation: Char => U, take: R => U) {
      assertEquals(0, closes)

      assertEquals(expectation('1'), take(unmanaged))
      assertEquals(1, creations)
      assertEquals(0, closes)

      val ex2 = expectation('2')
      assertEquals(ex2, take(unmanaged))
      assertEquals(1, creations)
      assertEquals(0, closes)
      closes = 0
      creations = 0
    }
  }

  @Test
  def unmanagedInput {
    val context = new InputContext()
    import scalax.io.JavaConverters._

    context.assertRead(context.in.asUnmanagedInput)(_.toByte, _.bytes.take(1).head)
    context.assertRead(Channels.newChannel(context.in).asUnmanagedInput)(_.toByte, _.bytes.take(1).head)
    context.assertRead(context.seekable(StandardOpenOption.ReadWrite: _*).asUnmanagedInput)(_.toByte, _.bytes.take(1).head)
    context.assertRead((new InputStreamReader(context.in)).asUnmanagedReadChars)(_.toChar, _.chars.take(1).head)
  }

  class OutputContext() {
    var closes = 0
    var creations = 0

    var byteArray = new ByteArrayOutputStream()
    def out = {
      byteArray = new ByteArrayOutputStream()
      new FilterOutputStream(byteArray) {
        creations += 1
        override def close() =
          closes += 1
      }
    }

    def assertWrite[R, U](unmanaged: R)(write: (Byte, R) => Unit) {
      assertEquals(0, closes)

      write(49, unmanaged)
      assertEquals(1, byteArray.toByteArray().length)
      assertEquals(49, byteArray.toByteArray()(0))
      assertEquals(1, creations)
      assertEquals(0, closes)

      write(55, unmanaged)
      assertEquals(2, byteArray.toByteArray().length)
      assertEquals(49, byteArray.toByteArray()(0))
      assertEquals(55, byteArray.toByteArray()(1))
      assertEquals(1, creations)
      assertEquals(0, closes)

      creations = 0
      closes = 0
    }

  }

  @Test
  def unmanagedOutput {
    val context = new OutputContext()
    import scalax.io.JavaConverters._

    context.assertWrite(context.out.asUnmanagedOutput)((i, r) => r.write(i))
    context.assertWrite(Channels.newChannel(context.out).asUnmanagedOutput)((i, r) => r.write(i))
    context.assertWrite(new OutputStreamWriter(context.out).asUnmanagedWriteChars)((i, r) => r.write(List(i.toChar)))
  }

}
