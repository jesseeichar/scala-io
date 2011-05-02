package scalaio.test

import collection.mutable.ArrayBuffer
import scalax.io.{Codec, Resource, ArrayBufferSeekableChannel, Seekable}
import scalax.io.StandardOpenOption.ReadWrite

class OpenableArrayBufferSeekableTest extends AbstractSeekableTests {
  def open(data: String) = {
    val buffer = ArrayBuffer[Byte]()
    buffer ++= data.getBytes(Codec.UTF8.charSet)
    Resource.fromSeekableByteChannel(options => new ArrayBufferSeekableChannel(buffer,options:_*)((),()))
  }
}

class SeekableArrayBufferSeekableTest extends AbstractSeekableTests {
  def open(data: String): Seekable = {
    val buffer = ArrayBuffer[Byte]()
    buffer ++= data.getBytes(Codec.UTF8.charSet)
    Resource.fromSeekableByteChannel(new ArrayBufferSeekableChannel(buffer,ReadWrite:_*)((),()))
  }
}