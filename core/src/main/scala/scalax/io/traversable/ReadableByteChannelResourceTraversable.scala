package scalax.io
package traversable
import java.io.Reader
import scalax.io.CloseableIterator
import scalax.io.LongTraversable
import scalax.io.LongTraversableLike
import scalax.io.OpenedResource
import java.nio.channels.ReadableByteChannel
import java.nio.{ ByteBuffer => NioByteBuffer }
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scalax.io.extractor.FileChannelExtractor
import scalax.io.nio.SeekableFileChannel
import java.io.Closeable
import scalax.io.support.FileUtils
import java.nio.channels.Channels
import java.io.ByteArrayOutputStream
import scalax.io.extractor.FileChannelExtractor

private[traversable] class ReadableByteChannelIterator(
  protected val sizeFunc: () => Option[Long],
  protected val getIn: ReadableByteChannel,
  protected val openResource: OpenedResource[Closeable],
  protected val start: Long,
  protected val end: Long) extends Sliceable {

  private[this] var buffer: NioByteBuffer = _
  private[this] var inConcrete: ReadableByteChannel = _

  private[this] val startIndex = start
  private[this] val endIndex = end

  private[this] var read = 0
  private[this] var i = 0
  private[this] var pos = start

  @inline
  final def init() = if (inConcrete == null) {
    val sliceLength: Long = (end - start) min Int.MaxValue
    val bufferSize = sizeFunc().map(_ min (sliceLength)).orElse(Some(sliceLength))
    buffer = openResource.context.createNioBuffer(bufferSize, Some(getIn), true)
    inConcrete = getIn
    skip(startIndex)
  }
  final def hasNext = {
    if (pos < endIndex && i < read) true
    else if (pos >= endIndex) {
      false
    } else {
      init()
      i = 0
      buffer.clear()
      read = inConcrete.read(buffer)
      i < read
    }
  }
  final def next = {
    i += 1
    pos += 1
    buffer.get(i - 1)
  }
  def doClose() = {
    pos = end;
    openResource.close()
  }

  @tailrec
  private final def skip(count: Long) {
    if (count > 0) {
      val toRead = (buffer.capacity.toLong min count).toInt

      buffer.clear()
      buffer.limit(toRead)

      val read = inConcrete read buffer
      if (read > -1) skip(count - read)
    }
  }

  def create(start: Long, end: Long) =
    new ReadableByteChannelIterator(sizeFunc, getIn, openResource, start, end)
}

private[traversable] class SeekableByteChannelIterator(
  val sizeFunc: () => Option[Long],
  val getIn: SeekableByteChannel,
  val openResource: OpenedResource[Closeable],
  val start: Long,
  val end: Long) extends Sliceable {
  private[this] val startIndex = start
  private[this] val endIndex = end
  private[this] var isInitialized = false
  private[this] var position = startIndex
  private[this] var buffer: NioByteBuffer = ByteBuffer.allocate(0)
  private[this] val channel = getIn
  @inline
  final def init() = if (!isInitialized) {
    channel.position(position)
    val sliceLength: Long = (end - start) min Int.MaxValue
    val bufferSize = sizeFunc().map(_ min (sliceLength)).orElse(Some(sliceLength))
    buffer = openResource.context.createNioBuffer(bufferSize, Some(getIn), true)
    isInitialized = true
  }
  final override def foreach[@specialized(Unit) U](f: Byte => U) =
    while (hasNext) f(next)

  def hasNext = {
    if (position < endIndex && buffer.hasRemaining) true
    else if (position >= endIndex) {
      false
    } else {
      init()
      channel.position(position)
      buffer.clear
      (channel read buffer)
      buffer.flip
      buffer.hasRemaining()
    }
  }
  def next = {
    position += 1
    buffer.get()
  }
  def doClose() = {
    position = end;
    openResource.close()
  }

  def create(start: Long, end: Long) =
    new SeekableByteChannelIterator(sizeFunc, getIn, openResource, start, end)

}
