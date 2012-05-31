package scalax.io
package traversable

import java.io.Closeable
import java.io.InputStream
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel

import scala.Option.option2Iterable

import scalax.io.extractor.FileChannelExtractor
import scalax.io.nio.SeekableFileChannel

/**
 * resourceOpener must either be a InputStream or a ReadableByteChannel (or subclass).  Anything else will throw an exception
 */
protected[io] class ByteResourceTraversable(
  resourceOpener: => OpenedResource[Closeable],
  resourceContext: ResourceContext,
  sizeFunc: () => Option[Long],
  start: Long,
  end: Long,
  allowSeekable: Boolean)
  extends LongTraversable[Byte]
  with LongTraversableLike[Byte, LongTraversable[Byte]] {
  self =>
  def context = resourceContext
  protected[io] def iterator: Sliceable = {
    val resource = resourceOpener
    resource.get match {
      case FileChannelExtractor(seekable) if allowSeekable =>
        new SeekableByteChannelIterator(sizeFunc, new SeekableFileChannel(seekable), resource, start, end)
      case seekable: SeekableByteChannel if allowSeekable =>
        new SeekableByteChannelIterator(sizeFunc, seekable, resource, start, end)
      case stream: InputStream =>
        new ReadableByteChannelIterator(sizeFunc, Channels.newChannel(stream), resource, start, end)
      case rbc: ReadableByteChannel =>
        new ReadableByteChannelIterator(sizeFunc, rbc, resource, start, end)
      case _ =>
        throw new AssertionError(getClass.getSimpleName + " only accepts inputStreams and readableByteChannels as input")
    }
  }

  override lazy val hasDefiniteSize = sizeFunc().nonEmpty
  override def lsize = {
    CloseableIterator.withIterator(iterator,context) {_.size}
  }

  override def toArray[B >: Byte: ClassManifest]: Array[B] = {
    CloseableIterator.withIterator(iterator,context) {_.toArray}
  }

}
