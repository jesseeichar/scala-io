package scalaio.test

import collection.mutable.ArrayBuffer
import scalax.io.{ Codec, Resource, ArrayBufferSeekableChannel, Seekable }
import scalax.io.StandardOpenOption.ReadWrite
import scalax.io._
import scalax.io.managed.SeekableByteChannelResource

class OpenableArrayBufferSeekableTest extends AbstractSeekableTests[SeekableByteChannel] with SeekableTestUtils[SeekableByteChannel] {
  var buffer = ArrayBuffer[Byte]()
  def deleteResource = buffer = null
  def openResource(closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    Resource.fromSeekableByteChannel(options => new ArrayBufferSeekableChannel(buffer, options:_*)(closeAction = s => closeAction(s)))
  }

  override def errorOnWriteOut = Resource.fromSeekableByteChannel(ExplodingSeekable)
}

object ExplodingSeekable extends ArrayBufferSeekableChannel(ArrayBuffer[Byte]())() {
  override def write(src: java.nio.ByteBuffer): Int = throw new Error("Boom")
  override def truncate(size: Long): scalax.io.SeekableByteChannel = throw new Error("Boom")
}
class SeekableArrayBufferSeekableTest extends AbstractSeekableTests[SeekableByteChannel] with SeekableTestUtils[SeekableByteChannel] {
  var buffer = ArrayBuffer[Byte]()
  def deleteResource = buffer = null
  def openResource(closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    Resource.fromSeekableByteChannel(new ArrayBufferSeekableChannel(buffer, StandardOpenOption.ReadWrite:_*)(closeAction = s => closeAction(s)))
  }

  override def errorOnWriteOut = Resource.fromSeekableByteChannel(ExplodingSeekable)
}