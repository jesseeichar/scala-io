/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{
    InputStream, OutputStream
}
import java.nio.ByteBuffer
import java.nio.channels.{
    ByteChannel, FileChannel, WritableByteChannel
}
import scalax.io.resource._
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
    private final val BUFFER_SIZE=8192

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
   * @param replaced
   *          The number of elements from bytes to replace.  If 
   *          -1 then all bytes will be used
   *          The stream will be grown as needed.
   *          Default is -1
   * @see patch(Long,Traversable[Byte],Iterable[OpenOption])
   */
  def patchString(position: Long, 
                  string: String,
                  replaced : Long = -1)(implicit codec: Codec): Unit = {
                    // TODO implement
                    assert(false, "not implemented")
                    ()
                  }

  /**
   * Update a portion of the file content with several bytes at
   * the declared location.
   * <p>
   * <strong>Important:</strong> The use of an Array is highly recommended
   * because normally arrays can be more efficiently written using
   * the underlying APIs
   * </p><p>
   * To append data the position must >= size
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
   *          The position must be within the file or == size (for appending)
   * @param bytes
   *          The bytes to write to the file starting at
   *          position.
   * @param replaced
   *          The number of elements from bytes to replace.  If 
   *          larger than bytes then all bytes will be used
   *          The stream will be grown as needed.
   *          Default is -1
   */
  def patch[T <% Traversable[Byte]](position: Long,
            bytes: T,
            replaced : Long = Long.MaxValue): Unit = {
    require(position >= 0, "The patch starting position must be greater than or equal 0")

    
    if(size.forall{position == _}) {
        replaced match {
            case -1 | _ if(replaced == bytes.size) =>
                append(bytes)
            case _ =>
                append(bytes, replaced)
        }
    } else {
        for(channel <- seekableChannel(WRITE) ){
            channel.position(position)
            writeTo(channel,bytes, replaced)
        }
    }
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
  * @param take
  *          The number of bytes to append -1 to take all
  *          default is -1
  */
  def append[T <% Traversable[Byte]](bytes: T, take : Long = -1): Unit = {
      for (c <- seekableChannel(APPEND)) writeTo(c, bytes, take)
  }

  private def writeTo[T <% Traversable[Byte]](c : WritableByteChannel, bytes : T, length : Long) = {
      bytes match {
          case array : Array[Byte] =>
          
              val count = if(length == -1) bytes.size else length.min(bytes.size)
              
              c.write(ByteBuffer.wrap(array, 0, count.toInt))
          case _ =>
              val buf = ByteBuffer.allocateDirect(BUFFER_SIZE)
              def write[T <% Traversable[Byte]](written : Long, data:T) : Unit = {
                  val numBytes = (length - written).min(BUFFER_SIZE).toInt
                  val (toWrite, remaining) = data.splitAt(numBytes)
                  
                  toWrite foreach buf.put
                  buf.flip
                  c write buf
                  
                  if (remaining.nonEmpty) write (written + toWrite.size, remaining)
              }
              write(0, bytes)
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
      append(string getBytes codec.name)
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
      assert (false, "not implemented")
  }
  
  def truncate(position : Long) : Unit = {
      assert (false, "not implemented")
  }
  
  // required methods for Input trait
  def chars(implicit codec: Codec): Traversable[Char] = seekableChannel(READ).reader(codec).chars
  def bytesAsInts:Traversable[Int] = seekableChannel(READ).bytesAsInts
  
  // required method for Output trait
  protected def outputStream = seekableChannel(WRITE_TRUNCATE:_*).outputStream
  
}
