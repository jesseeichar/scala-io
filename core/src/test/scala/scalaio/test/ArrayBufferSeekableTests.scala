package scalaio.test

import collection.mutable.ArrayBuffer
import scalax.io.{Codec, Resource, ArrayBufferSeekableChannel, Seekable}
import scalax.io.StandardOpenOption.ReadWrite
import scalax.io._

class OpenableArrayBufferSeekableTest extends AbstractSeekableTests[SeekableByteChannel] {
  def open(data: String, closeAction:CloseAction[SeekableByteChannel] = CloseAction.Noop) = {
    val buffer = ArrayBuffer[Byte]()
    buffer ++= data.getBytes(Codec.UTF8.charSet)
    new SeekableByteChannelResource(options => new ArrayBufferSeekableChannel(buffer,options:_*)(_=>(),_=>()), closeAction, () => Some(buffer.size))
  }
}

class SeekableArrayBufferSeekableTest extends AbstractSeekableTests[SeekableByteChannel] {
  def open(data: String, closeAction:CloseAction[SeekableByteChannel] = CloseAction.Noop): Seekable = {
    val buffer = ArrayBuffer[Byte]()
    buffer ++= data.getBytes(Codec.UTF8.charSet)
    new SeekableByteChannelResource(_ => new ArrayBufferSeekableChannel(buffer,ReadWrite:_*)(_=>(),_=>()), closeAction, () => Some(buffer.size))
  }
}