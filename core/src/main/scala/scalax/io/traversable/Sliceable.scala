package scalax.io.traversable

import scalax.io.CloseableIterator
import scalax.io.support.Misc.safeSum
import scalax.io.extractor.FileChannelExtractor
import scalax.io.support.FileUtils
import java.nio.channels.Channels
import java.io.ByteArrayOutputStream
import java.nio.channels.ReadableByteChannel
import java.io.InputStream

/**
 * Note: This is for implementation if subclases like the InputStreamIterator iterator
 * are accessed directly somehow and iter.next.drop(5).next will have wrong result
 */
private[io] abstract class Sliceable extends CloseableIterator[Byte] {

  protected def create(start: Long, end: Long): Sliceable

  protected val start: Long
  protected val end: Long
  /**
   * Note: should only init if a previous init has taken place
   */
  def init(): Unit
  override def slice(from: Int, until: Int): Sliceable = lslice(start, end)
  override def lslice(from: Long, until: Long): Sliceable = {
    val newStart = start + (from max 0)
    val newEnd = if (until > end) end
    else safeSum(start, (until max 0))

    create(newStart, newEnd)
  }

  protected def sizeFunc: () => Option[Long]
  /** Return the underlying ReadableByteChannel or InputStream.  any other object is an error*/
  protected def getIn: Any

  override def size = lsize min Integer.MAX_VALUE toInt
  def lsize = {
    def fileSize = sizeFunc() match {
      case Some(size) => size min (end - start)
      case None =>
          var result = 0L
            while(hasNext){
              result += 1
              next
            }
            result
    }

    init()
    val sliceLength: Long = (end - start) min Int.MaxValue
    getIn match {
      case FileChannelExtractor(fc) =>
        sliceLength min fc.size
      case _ =>
        sliceLength min fileSize
    }
  }

  /*  TODO reimplement override when I can
  override def toArray[B >: Byte: ClassManifest]: Array[B] = {
    init()
    val in = getIn
    val sliceLength: Long = (end - start) min Int.MaxValue
    in match {
      case FileChannelExtractor(fc) =>
        val out = new ByteArrayOutputStream((sliceLength min fc.size min Int.MaxValue).toInt) {
          def getBuffer = buf
        }
        FileUtils.copy(fc, Channels.newChannel(out))
        out.getBuffer.asInstanceOf[Array[B]]
      case c: ReadableByteChannel =>
        val out = new ByteArrayOutputStream()
        FileUtils.copy(c, Channels.newChannel(out))
        out.toByteArray().asInstanceOf[Array[B]]
      case stream: InputStream =>
        val out = new ByteArrayOutputStream()
        FileUtils.copy(stream, out)
        out.toByteArray().asInstanceOf[Array[B]]
      case _ => throw new IllegalStateException(in.getClass+" is neither a ReadableByteChannel or an InputStream, there is a problem with the implementation because only one of those two should be return by getIn")
    }
  }*/
}
