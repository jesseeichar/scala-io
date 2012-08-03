package scalaio.test

import collection.mutable.ArrayBuffer
import scalax.io.{Codec, Resource, ArrayBufferSeekableChannel, Seekable}
import scalax.io.StandardOpenOption.ReadWrite
import scalax.io._
import scalax.io.managed.SeekableByteChannelResource
import java.nio.ByteBuffer
import org.junit.Assert._
import java.nio.channels.SeekableByteChannel

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
  override def truncatesUnderlyingSinkEachOpen = true
  override def errorOnWriteOut = Resource.fromSeekableByteChannel(ExplodingSeekable)
}

object ExplodingSeekable extends ArrayBufferSeekableChannel(ArrayBuffer[Byte]())() {
  override def read(dst: ByteBuffer) = throw new Error("Boom")

  override def position(newPosition: Long) = throw new Error("Boom")

  override def write(src: java.nio.ByteBuffer): Int = throw new Error("Boom")

  override def truncate(size: Long): SeekableByteChannel = throw new Error("Boom")
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
  override def truncatesUnderlyingSinkEachOpen = true

}
