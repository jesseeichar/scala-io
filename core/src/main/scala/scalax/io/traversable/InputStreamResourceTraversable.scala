package scalax.io
package traversable
import java.io.InputStream
import scalax.io.support.FileUtils
import java.io.ByteArrayOutputStream
import java.io.Closeable

private[traversable] class InputStreamIterator(
  protected val sizeFunc: () => Option[Long],
  protected val getIn: InputStream,
  protected val openResource: OpenedResource[Closeable],
  protected val start: Long,
  protected val end: Long) extends Sliceable {

  private[this] var inConcrete: InputStream = null
  private[this] var buffer: Array[Byte] = _

  private[this] val startIndex = start
  private[this] val endIndex = end

  private[this] var read = Integer.MIN_VALUE
  private[this] var i = 0
  private[this] var pos = start
  @inline
  final def init() = if (inConcrete == null) {
    val sliceLength: Long = (end - start) min Int.MaxValue
    buffer = Buffers.arrayBuffer(sizeFunc().map(_ min (sliceLength)).orElse(Some(sliceLength)))
    inConcrete = getIn
    inConcrete.skip(startIndex)
  }

  final def hasNext = {
    if (pos < endIndex && i < read) true
    else if (pos >= endIndex) {
      false
    } else {
      init()
      i = 0
      read = inConcrete.read(buffer)
      i < read
    }
  }
  final def next = {
    i += 1
    pos += 1
    buffer(i - 1)
  }
  def doClose() = {
    pos = endIndex
    openResource.close()
  }

  def create(start: Long, end: Long) = new InputStreamIterator(sizeFunc, getIn, openResource, start, end)
}