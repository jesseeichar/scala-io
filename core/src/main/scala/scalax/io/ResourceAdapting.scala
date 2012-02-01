package scalax.io

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.channels.{ByteChannel, ReadableByteChannel, WritableByteChannel, Channels}
import java.io._
import java.lang.String

/**
 * Supporting classes for converting between resource types.  For example if you want to convert an InputStream
 * to a Reader you need to use a InputStreamReader but the Closeactions for the InputStream still need to be
 * executed on close.  So a reference to the InputStream must be kept.  The solution is to create an
 * `InputStreamReader with ResourceAdapting.Adapter`  The resource adapter will keep the reference to the underlying
 * Resource and a new CloseAction can be created that takes a ResourceAdapter and the original CloseAction and
 * executes the CloseAction on the referenced resource from the ResourceAdapter
 *
 * '''Not part of the API.'''
 *
 * @tparam S the type of the object before adapting to another type
 */
trait Adapter[+S] {
  def src: S
}

/**
 * Contains a method
 *
 *
 * '''Not part of the API.'''
 *
 */
protected[io] object ResourceAdapting {
  /**
   * Creates a CloseAction that is an bridge/adapter of a CloseAction
   * based on Resource[S] to so it works work on a Resource[C] that is
   * also a ResourceAdapting.Adapter[S]
   *
   * @see [scalax.io.Adapter]
   */
  def closeAction[A](src:CloseAction[A]):CloseAction[Adapter[A]]  = {
    src match {
      case CloseAction.Noop =>
        CloseAction.Noop
      case _ =>
        CloseAction((in:Adapter[A]) => src(in.src))
    }
  }

  /**
   * Adapts an OutputStream to a WritableByte Channel.
   *
   * '''Not part of the API.'''
   */
  protected[io] class WritableChannelAdapter[+A <: OutputStream](opener: => A) extends WritableByteChannel with Adapter[A] {
    lazy val src = opener
    lazy val channel = Channels.newChannel(src)

    def close = channel.close()
    def isOpen = channel.isOpen()
    def write(p1: ByteBuffer) = channel.write(p1)
  }
  /**
   * Adapts an OutputStream to a WritableByte Channel.
   *
   * '''Not part of the API.'''
   */
  protected[io] class ReadableChannelAdapter[+A <: InputStream](opener: => A, managed:Boolean) extends ReadableByteChannel with Adapter[A] {
    lazy val src = opener
    lazy val channel = Channels.newChannel(src)

    def close = if(managed) channel.close()
    def isOpen = channel.isOpen()

    def read(p1: ByteBuffer) = channel.read(p1)
  }

  /**
   * Adapts an ReadableByteChannel to a Reader
   *
   * '''Not part of the API.'''
   */
  protected[io] class ChannelReaderAdapter[+In <: ReadableByteChannel](opener: => In,codec:Codec, managed:Boolean) extends Reader with Adapter[In] {
    lazy val src = opener
    lazy val reader = Channels.newReader(src, codec.name)
    def read(p1: Array[Char], p2: Int, p3: Int) = reader.read(p1,p2,p3)
    override def reset = reader.reset
    override def mark(p1: Int) = reader.mark(p1)
    override def markSupported = reader.markSupported
    override def ready = reader.ready
    override def skip(p1: Long) = reader.skip(p1)
    override def read(p1: Array[Char]) = reader.read(p1)
    override def read = reader.read
    override def read(p1: CharBuffer) = reader.read(p1)

    def close = if (managed) reader.close();
  }
  /**
   * Adapts an ReadableByteChannel to a InputStream.
   *
   * '''Not part of the API.'''
   */
  protected[io] class ChannelInputStreamAdapter[+In <: ReadableByteChannel](opener: => In, managed:Boolean) extends InputStream with Adapter[In] {
    lazy val src = opener
    lazy val stream = Channels.newInputStream(src)
    override def read(p1: Array[Byte]) = stream.read(p1)
    override def close = if(managed) stream.close
    override def markSupported = stream.markSupported
    override def reset = stream.reset
    override def mark(p1: Int) = stream.mark(p1)
    override def available = stream.available
    override def skip(p1: Long) = stream.skip(p1)
    override def read(p1: Array[Byte], p2: Int, p3: Int) = stream.read(p1,p2,p3)
    override def read = stream.read
  }
  /**
   * Adapts an WritableByteChannel to a Writer.
   *
   * '''Not part of the API.'''
   */
  protected[io] class ChannelWriterAdapter[+In <: WritableByteChannel](opener: => In,codec:Codec, managed:Boolean) extends Writer with Adapter[In] {
    lazy val src = opener
    lazy val writer = Channels.newWriter(src,codec.name)

    def close = if (managed) writer.close()
    def flush = writer.flush()
    def write(p1: Array[Char], p2: Int, p3: Int) = writer.write(p1,p2,p3)

    override def append(p1: Char) = writer.append(p1)
    override def append(p1: CharSequence, p2: Int, p3: Int) = writer.append(p1,p2,p3)
    override def append(p1: CharSequence) = writer.append(p1)
    override def write(p1: String, p2: Int, p3: Int) = writer.write(p1,p2,p3)
    override def write(p1: String) = writer.write(p1)
    override def write(p1: Array[Char]) = writer.write(p1)
    override def write(p1: Int) = writer.write(p1)
  }
  /**
   * Adapts an WritableByteChannel to a OutputStream.
   *
   * '''Not part of the API.'''
   */
  protected[io] class ChannelOutputStreamAdapter[+In <: WritableByteChannel](opener: => In, managed:Boolean) extends OutputStream with Adapter[In] {
    lazy val src = opener
    lazy val out = Channels.newOutputStream(src)

    def write(p1: Int) = out.write(p1)

    override def close = if(managed) out.close()
    override def flush = out.flush()
    override def write(p1: Array[Byte], p2: Int, p3: Int) = out.write(p1,p2,p3)
    override def write(p1: Array[Byte]) = out.write(p1)
  }

}
