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

    def assertRead[R <: Resource[_], U](unmanaged: R)(expectation: Char => U, take: R => U) {
      assertEquals(0, closes)

      assertEquals(expectation('1'), take(unmanaged))
      assertEquals(1, creations)
      assertEquals(0, closes)

      val ex2 = expectation('2')
      assertEquals(ex2, take(unmanaged))
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.acquireAndGet(_ => ())
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.acquireFor(_ => ())
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.asInstanceOf[UnmanagedResource].close()
      assertEquals(1, creations)
      assertEquals(1, closes)
      closes = 0
      creations = 0
    }

    def assertSeekable(unmanaged: Seekable with Resource[_] with UnmanagedResource) {
      assertEquals(0, closes)

      assertEquals('1'.toByte, unmanaged.bytes.take(1).head)
      assertEquals(1, creations)
      assertEquals(0, closes)

      assertEquals('2'.toByte, (unmanaged.bytes drop 1).head)
      assertEquals(1, creations)
      assertEquals(0, closes)

      assertEquals('2', (unmanaged.chars drop 1).head)
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.truncate(0)
      assertEquals(None, unmanaged.bytes.headOption)
      assertEquals(0, unmanaged.bytes.size)
      assertEquals(Some(0), unmanaged.size)
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.append(List[Byte](1, 2, 3))
      assertEquals(Some(3), unmanaged.size)
      assertEquals(1, creations)
      assertEquals(0, closes)

      assertEquals(2, (unmanaged.bytes drop 1).head)
      assertEquals(1, unmanaged.bytes.head)
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.insert(1, List[Byte](8))
      assertEquals(Some(4), unmanaged.size)
      assertEquals(1, creations)
      assertEquals(0, closes)

      assertEquals(8, (unmanaged.bytes drop 1).head)
      assertEquals(1, unmanaged.bytes.head)
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.acquireFor(_ => ())
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.acquireAndGet(_ => 1)
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.close()
      assertEquals(1, creations)
      assertEquals(1, closes)
      closes = 0
      creations = 0
    }

  }

  @Test
  def unmanagedInput {
    val context = new InputContext()
    def assertInputResource(managed: InputResource[Closeable]) {
      context.assertRead(managed.unmanaged)(_.toByte, _.bytes.take(1).head)
      context.assertRead(managed.unmanaged.inputStream)(_.toByte, _.bytes.take(1).head)
      context.assertRead(managed.unmanaged.inputStream.inputStream)(_.toByte, _.bytes.take(1).head)
      context.assertRead(managed.unmanaged.inputStream.readableByteChannel)(_.toByte, _.bytes.take(1).head)
      context.assertRead(managed.unmanaged.readableByteChannel)(_.toByte, _.bytes.take(1).head)
      context.assertRead(managed.unmanaged.readableByteChannel.inputStream)(_.toByte, _.bytes.take(1).head)
      context.assertRead(managed.unmanaged.readableByteChannel.readableByteChannel)(_.toByte, _.bytes.take(1).head)
      context.assertRead(managed.unmanaged.reader())(_.toChar, _.chars.take(1).head)
    }

    assertInputResource(Resource.fromInputStream(context.in))
    assertInputResource(Resource.fromReadableByteChannel(Channels.newChannel(context.in)))
    assertInputResource(Resource.fromByteChannel(context.seekable(StandardOpenOption.ReadWrite: _*)))
  }

  @Test
  def unmanagedSeekable {
    val context = new InputContext()

    val resource = Resource.fromSeekableByteChannel(ops => context.seekable(ops: _*))
    /*    context.assertRead(resource.unmanaged.inputStream)(_.toByte, _.bytes.take(1).head)
    context.assertRead(resource.unmanaged.inputStream.inputStream)(_.toByte, _.bytes.take(1).head)
    context.assertRead(resource.unmanaged.inputStream.readableByteChannel)(_.toByte, _.bytes.take(1).head)
    context.assertRead(resource.unmanaged.readableByteChannel)( _.toByte, _.bytes.take(1).head)
    context.assertRead(resource.unmanaged.readableByteChannel.inputStream)( _.toByte, _.bytes.take(1).head)
    context.assertRead(resource.unmanaged.readableByteChannel.readableByteChannel)( _.toByte, _.bytes.take(1).head)
    context.assertRead(resource.unmanaged.reader())( _.toChar, _.chars.take(1).head)    */
    context.assertSeekable(resource.unmanaged)
  }

  @Test
  def unmanagedReadChars {
    val context = new InputContext()
    def assertInputResource(managed: ReadCharsResource[Closeable]) {
      context.assertRead(managed.unmanaged)(_.toChar, _.chars.take(1).head)
    }

    assertInputResource(Resource.fromInputStream(context.in).reader())
    assertInputResource(Resource.fromReadableByteChannel(Channels.newChannel(context.in)).reader())
    assertInputResource(Resource.fromByteChannel(context.seekable(StandardOpenOption.ReadWrite: _*)).reader())
    assertInputResource(Resource.fromSeekableByteChannel(ops => context.seekable(ops: _*)).reader())
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
    var seekabledata = new ArrayBuffer[Byte]()
    def seekable(openOptions: OpenOption*) = {
      seekabledata.clear()
      creations += 1
      new ArrayBufferSeekableChannel(seekabledata, openOptions: _*)(closeAction = _ => closes += 1)
    }

    def assertWrite[R <: Resource[_], U](unmanaged: R)(write: (Byte, R) => Unit) {
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

      unmanaged.acquireAndGet(_ => ())
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.acquireFor(_ => ())
      assertEquals(1, creations)
      assertEquals(0, closes)

      unmanaged.asInstanceOf[UnmanagedResource].close()
      assertEquals(1, creations)
      assertEquals(1, closes)
      closes = 0
      creations = 0
    }

  }

  @Test
  def unmanagedOutput {
    val context = new OutputContext()
    def assertInputResource(managed: OutputResource[Closeable]) {
      context.assertWrite(managed.unmanaged)((i, r) => r.write(i))
      context.assertWrite(managed.unmanaged.writableByteChannel)((i, r) => r.write(i))
      context.assertWrite(managed.unmanaged.writableByteChannel.writableByteChannel)((i, r) => r.write(i))
      context.assertWrite(managed.unmanaged.writableByteChannel.outputStream)((i, r) => r.write(i))
      context.assertWrite(managed.unmanaged.outputStream)((i, r) => r.write(i))
      context.assertWrite(managed.unmanaged.outputStream.outputStream)((i, r) => r.write(i))
      context.assertWrite(managed.unmanaged.outputStream.writableByteChannel)((i, r) => r.write(i))
      context.assertWrite(managed.unmanaged.writer())((i, r) => r.write(List(i.toChar)))
    }

    assertInputResource(Resource.fromOutputStream(context.out))
    assertInputResource(Resource.fromOutputStream(context.out).writableByteChannel)
    assertInputResource(Resource.fromWritableByteChannel(Channels.newChannel(context.out)))
  }

}
