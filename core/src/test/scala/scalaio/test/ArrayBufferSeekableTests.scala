package scalaio.test

import collection.mutable.ArrayBuffer
import scalax.io.{Codec, Resource, ArrayBufferSeekableChannel, Seekable}
import scalax.io.StandardOpenOption.ReadWrite
import scalax.io._
import scalax.io.managed.SeekableByteChannelResource
import java.nio.ByteBuffer
import org.junit.Assert._

class OpenableArrayBufferSeekableTest extends AbstractSeekableTests[SeekableByteChannel] with SeekableTestUtils[SeekableByteChannel] {
  var buffer = ArrayBuffer[Byte]()

  def forceErrorOnAccess = buffer = null

  def openResource(openFunction: () => Unit, closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    def boomIfFails(s:SeekableByteChannel) = closeAction(s) match
    {
      case e :: l => throw e
      case Nil => ()
    }
    buffer.clear()
    def seekable(options: Seq[OpenOption]) = new ArrayBufferSeekableChannel(buffer, options: _*)(closeAction = boomIfFails _) {
      openFunction()
    }
    Resource.fromSeekableByteChannel(options => seekable(options))
  }

  override def writeErrorRaisedOnClose = true

  override def errorOnWriteOut = Resource.fromSeekableByteChannel(ExplodingSeekable)
}

object ExplodingSeekable extends ArrayBufferSeekableChannel(ArrayBuffer[Byte]())() {
  override def read(dst: ByteBuffer) = throw new Error("Boom")

  override def position(newPosition: Long) = throw new Error("Boom")

  override def read(dst: ByteBuffer, pos: Long) = throw new Error("Boom")

  override def write(src: ByteBuffer, pos: Long) = throw new Error("Boom")

  override def write(src: java.nio.ByteBuffer): Int = throw new Error("Boom")

  override def truncate(size: Long): scalax.io.SeekableByteChannel = throw new Error("Boom")
}

class SeekableArrayBufferSeekableTest extends AbstractSeekableTests[SeekableByteChannel] with SeekableTestUtils[SeekableByteChannel] {
  var buffer = ArrayBuffer[Byte]()

  def forceErrorOnAccess = buffer = null
  override def writeErrorRaisedOnClose = true

  def openResource(openFunction: () => Unit, closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    buffer.clear()
    def boomIfFails(s:SeekableByteChannel) = closeAction(s) match
    {
      case e :: l => throw e
      case Nil => ()
    }
    def seekable = new ArrayBufferSeekableChannel(buffer, StandardOpenOption.ReadWrite: _*)(closeAction = boomIfFails _) {
      openFunction()
    }
    Resource.fromSeekableByteChannel(options => seekable)
  }

  override def errorOnWriteOut = Resource.fromSeekableByteChannel(ExplodingSeekable)

  /**
   * Overriding because this resource cannot append
   */
  override def openOutput: Unit = {
    var closes = 0;
    val (in,out) = open(CloseAction((c:Any) => closes += 1))
    out match {
      case out:OutputResource[_] =>

        assertEquals(0,closes)
        out.write("whoop!")
        assertEquals(1,closes)

        for (opened <- out.outputProcessor) {
          opened.write("hello")
          opened.write(" ")
          opened.write("world")
        }
        assertEquals(2,closes)
        assertEquals("hello world",in.slurpString)
      case _ => ()
    }
  }

}