/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io.Codec

import java.io.{
  BufferedReader, BufferedWriter, InputStream, OutputStream,
  BufferedInputStream, BufferedOutputStream, Reader, Writer,
  Closeable, InputStreamReader, OutputStreamWriter
}
import java.nio.channels.{
  ByteChannel, ReadableByteChannel, WritableByteChannel,
  Channels, FileChannel
}
import java.util.logging.{
    Logger, Level
}

/**
 * Defines several factory methods for creating instances of Resource.
 * It also defines several useful Resource types. For example
 * ResourceType which is a Resource[Reader] with Bufferable[BufferedReader].
 * All the types that can be created with the factory methods can be created.
 * <p>
 * Example usage:
 * <pre><code>
 * val URL = new URL("http://scala-lang.org")
 * val resource: Resource[InputStream] = Resource.fromInputStream(url.openStream).buffered
 * </code></pre>
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
object Resource {

  // InputStream factory methods
  /**
   * Create an Input Resource instance with several conversion traits from an InputStream.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new InputStream
   * @return
   *          an InputResource
   */
  def fromInputStream[A <: InputStream](opener: => A) : InputStreamResource[A] = new InputStreamResource[A](opener)
  /**
   * Create an Input Resource instance from a BufferedInputStream
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedInputStream
   *
   * @return
   *          a InputStreamResource that is backed by a BufferedInputStream
   */
  def fromBufferedInputStream[A <: BufferedInputStream](opener: => A) : InputStreamResource[A]  = new InputStreamResource[A](opener){
      override def buffered = this;
  }

  // OutputStream factory methods
  /**
   * Create an Output Resource instance with several conversion traits from an OutputStream.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new OutputStream
   *
   * @return
   *          an OutputStreamResource
   */
  def fromOutputStream[A <: OutputStream](opener: => A) : OutputStreamResource[A] = new OutputStreamResource[A](opener)
  /**
   * Create an Output Resource instance from a BufferedOutputStream
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedOutputStream
   *
   * @return
   *          a OutputStreamResource that is backed by a BufferedOutputStream
   */
  def fromBufferedOutputStream[A <: BufferedOutputStream](opener: => A) : OutputStreamResource[A] = new OutputStreamResource[A](opener) {
      override def buffered = this
  }

  // Reader factory methods
  /**
   * Create an Input Resource instance with conversion traits from an Reader.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new Reader
   *          this is only important if the stream may be converted to a
   * @param codec
   *          the codec representing the encoding of the strings that the Reader will Read
   *
   * @return
   *          an ReaderResource
   */
  def fromReader[A <: Reader](opener: => A)(implicit codec: Codec) : ReaderResource[A] = new ReaderResource[A](opener, codec)
  /**
   * Create an Input Resource instance with conversion traits from an BufferedReader.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedReader
   * @param codec
   *          the codec representing the encoding of the strings that the Reader will Read
   *
   * @return
   *          a ReaderResource that is backed by a BufferedReader
   */
  def fromBufferedReader[A <: BufferedReader](opener: => A)(implicit codec: Codec): ReaderResource[A] = new ReaderResource[A](opener, codec) {
      override def buffered = this
  }

  // Writer factory methods
  /**
   * Create an Output Resource instance with conversion traits from an Writer.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new Writer
   * @param codec
   *          the codec representing the encoding of the strings that the writer will write out
   *
   * @return
   *          an WriterResource
   */
  def fromWriter[A <: Writer](opener: => A)(implicit codec: Codec) : WriterResource[A] = new WriterResource[A](opener, codec)
  /**
   * Create an Output Resource instance with conversion traits from an BufferedWriter.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * Out other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new BufferedWriter
   * @param codec
   *          the codec representing the encoding of the strings that the writer will write out
   * @return
   *          a WriterResource that is backed by a BufferedWriter
   */
  def fromBufferedWriter[A <: BufferedWriter](opener: => A)(implicit codec: Codec): WriterResource[A] = new WriterResource[A](opener, codec) {
      override def buffered = this
  }

  // Channel factory methods
  /**
   * Create an Input Resource instance with several conversion traits from an ReadableByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new ReadableByteChannel
   *
   * @return
   *          an ReadableByteChannelResource
   */
  def fromReadableByteChannel[A <: ReadableByteChannel](opener: => A) : ReadableByteChannelResource[A] = new ReadableByteChannelResource[A](opener)
  /**
   * Create an Output Resource instance with several conversion traits from an WritableByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new WritableByteChannel
   *
   * @return
   *          an WritableByteChannelResource
   */
  def fromWritableByteChannel[A <: WritableByteChannel](opener: => A) : WritableByteChannelResource[A] = new WritableByteChannelResource[A](opener)
  /**
   * Create an Input/Output Resource instance with several conversion traits from an ByteChannel.
   * <p>
   * The opener param is a by-name argument an is use to open a new stream.
   * In other words it is important to try and pass in a function for opening
   * the stream rather than the already openned stream so that the returned
   * Resource can be used multiple time
   * </p>
   *
   * @param opener
   *          the function for opening a new ByteChannel
   *
   * @return
   *          an ByteChannelResource
   */
  def fromByteChannel[A <: ByteChannel](opener: => A) : ByteChannelResource[A] = new ByteChannelResource[A](opener)
}
