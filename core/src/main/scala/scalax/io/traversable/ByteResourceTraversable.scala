package scalax.io.traversable
import scalax.io.LongTraversableLike
import scalax.io.LongTraversable
import scalax.io.OpenedResource
import java.nio.channels.ReadableByteChannel
import scalax.io.extractor.FileChannelExtractor
import java.io.Closeable
import scalax.io.nio.SeekableFileChannel
import scalax.io.SeekableByteChannel
import java.io.InputStream
import java.nio.channels.Channels

/**
 * resourceOpener must either be a InputStream or a ReadableByteChannel (or subclass).  Anything else will throw an exception
 */
protected[io] class ByteResourceTraversable(
  resourceOpener: => OpenedResource[Closeable],
  sizeFunc: () => Option[Long],
  start: Long,
  end: Long,
  allowSeekable: Boolean)
  extends LongTraversable[Byte]
  with LongTraversableLike[Byte, LongTraversable[Byte]] {
  self =>

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
    val iter = iterator
    try iter.size
    finally iter.close
  }

  override def toArray[B >: Byte: ClassManifest]: Array[B] = {
    val iter = iterator
    try iter.toArray
    finally iter.close
  }

}