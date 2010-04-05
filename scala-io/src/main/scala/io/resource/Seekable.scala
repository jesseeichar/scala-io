/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import java.io.{
    InputStream, OutputStream
}
import java.nio.channels.{
    ByteChannel, FileChannel
}
import scalax.io.{
    OpenOption, Codec
}
import scala.collection.Traversable
import OpenOption._
import Resource._

/**
 * An object for reading and writing to Random Access IO objects such as Files.
 *
 * @author Jesse Eichar
 * @since 1.0
 */
trait Seekable extends Input with Output {

    // for Java 7 change this to a Seekable Channel
    /**
     * The underlying channel to write to.  The open options indicate the preferred way
     * to interact with the underlying channel.
     */
    protected def seekableChannel(openOptions:OpenOption*) : OutputResource[FileChannel] with InputResource[FileChannel]

  /**
   * Update a portion of the file content with string at
   * the declared location.
   * <p>
   * If the position is beyond the end of the file a BufferUnderflow
   * Exception will be thrown
   * </p><p>
   * If the position is within the file but the
   * <code>position + string.getBytes(codec).length</code>
   * is beyond the end of the file the file will be enlarged so
   * that the entire string can fit in the file
   * </p><p>
   * The write begins at the position indicated.  So if position = 0
   * then the write will begin at the first byte of the file.
   * </p>
   * @param position
   *          The start position of the update starting at 0.
   *          The position must be within the file
   * @param string
   *          The string to write to the file starting at
   *          position.
   * @param openOptions
   *          The options to use for opening the file
   *          Default is WRITE
   * @param replaced
   *          The number of elements from bytes to replace.  If 
   *          larger than bytes then all bytes will be used
   *          The stream will be grown as needed.
   *          Default is Long.MaxValue
   * @see patch(Long,Traversable[Byte],Iterable[OpenOption])
   */
  def patchString(position: Long, 
                  string: String,
                  replaced : Long = Long.MaxValue,
                  openOptions: Iterable[OpenOption] = List(WRITE))(implicit codec: Codec): Unit = {
                    // TODO implement
                    ()
                  }

  /**
   * Update a portion of the file content with several bytes at
   * the declared location.
   * <p>
   * <strong>Important:</strong> The use of an Array is highly recommended
   * because normally arrays can be more efficiently written using
   * the underlying APIs
   * </p>
   * <p>
   * If the position is beyond the end of the file a BufferUnderflow
   * Exception will be thrown
   * </p><p>
   * If the position is within the file but the
   * <code>position + bytes.length</code>
   * is beyond the end of the file the file will be enlarged so
   * that the entire string can fit in the file
   * </p><p>
   * The write begins at the position indicated.  So if position = 0
   * then the write will begin at the first byte of the file.
   * </p>
   * @param position
   *          The start position of the update starting at 0.
   *          The position must be within the file
   * @param bytes
   *          The bytes to write to the file starting at
   *          position.
   * @param replaced
   *          The number of elements from bytes to replace.  If 
   *          larger than bytes then all bytes will be used
   *          The stream will be grown as needed.
   *          Default is Long.MaxValue
   * @param openOptions
   *          The options to use for opening the file
   *          Default is WRITE
   */
  def patch(position: Long,
            bytes: Traversable[Byte],
            replaced : Long = Long.MaxValue,
            openOptions: Iterable[OpenOption] = List(WRITE)): Unit = {
     require(position >= 0, "The patch starting position must be within the existing file")
                    // TODO implement
                    ()
  }

  /**
  * Append bytes to the end of a file
  *
  * <strong>Important:</strong> The use of an Array is highly recommended
  * because normally arrays can be more efficiently written using
  * the underlying APIs
  *
  * @param bytes
  *          The bytes to write to the file
  */
  def appendBytes(bytes: Traversable[Byte]): Unit = {
      for (out <- outputStream) {
          bytes foreach {i => out write i.toInt}
      }
  }

  /**
  * Writes a string. The open options that can be used are dependent
  * on the implementation and implementors should clearly document
  * which option are permitted.
  * 
  * @param string
  *          the data to write
  * @param codec
  *          the codec of the string to be written. The string will
  *          be converted to the encoding of {@link sourceCodec}
  *          Default is sourceCodec
  */
  def appendString(string: String)(implicit codec: Codec): Unit = {
      // TODO
      ()
  }

  /**
  * Write several strings. The open options that can be used are dependent
  * on the implementation and implementors should clearly document
  * which option are permitted.
  * 
  * @param strings
  *          The data to write
  * @param separator
  *          A string to add between each string.  
  *          It is not added to the before the first string
  *          or after the last.
  * @param codec
  *          The codec of the strings to be written. The strings will
  *          be converted to the encoding of {@link sourceCodec}
  *          Default is sourceCodec
  */  
  def appendStrings(strings: Traversable[String], separator:String = "")(implicit codec: Codec): Unit = {
      // TODO
      ()
  }
  
  // required methods for Input trait
  def chars(implicit codec: Codec): Traversable[Char] = seekableChannel(READ).reader(codec).chars
  def bytesAsInts:Traversable[Int] = seekableChannel(READ).bytesAsInts
  
  // required method for Output trait
  protected def outputStream = seekableChannel(WRITE_TRUNCATE:_*).outputStream
  
}
