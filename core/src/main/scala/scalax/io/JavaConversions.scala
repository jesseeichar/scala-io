/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.io.nio.ByteBuffer
import java.net.URL
import java.io.{InputStream, Closeable, RandomAccessFile, File}

object JavaConversions {
  class AsResource[R <: Resource[_]](op: => R) {
    def asResource = op
  }
  implicit def asResource(url:URL): AsResource[InputStreamResource[InputStream]] =
    new AsResource(Resource.fromInputStream(url.openStream))
  implicit def asResource(file:File):AsResource[SeekableByteChannelResource[SeekableByteChannel]] =
    asResource(new RandomAccessFile(file,"rw"))
  implicit def asResource(randomAccessFile:RandomAccessFile):AsResource[SeekableByteChannelResource[SeekableByteChannel]] =
    new AsResource(Resource.fromRandomAccessFile(randomAccessFile))
  implicit def byteBufferToTraversable(b:java.nio.ByteBuffer): ByteBuffer = new ByteBuffer(b)
}
