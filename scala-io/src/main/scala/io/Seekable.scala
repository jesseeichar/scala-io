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
    private final val BufferSize=8192
    private final val MaxPermittedInMemory=8192
    

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
   *          The number of elements from bytes to replace.  
   *          The stream will be grown as needed.
   *          Default will use all bytes in patch
   * @see patch(Long,Traversable[Byte],Iterable[OpenOption])
   */
  def patchString(position: Long, 
                  string: String,
                  replaced : Long = -2)(implicit codec: Codec): Unit = {
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
   *          Default will use all bytes in patch
   */
  def patch[T <% Traversable[Byte]](position: Long,
            bytes: T,
            replaced : Long = -2): Unit = {
    require(position >= 0, "The patch starting position must be greater than or equal 0")

    val appendData = size.forall{position == _}
    val insertData = replaced == -1
        
    if(appendData) {
        append(bytes, replaced)
    } else if(insertData) {
        if (bytes.hasDefiniteSize && bytes.size <= MaxPermittedInMemory) {
            insertDataInMemory(position, bytes)
        } else {
            insertDataTmpFile(position, bytes)
        }
    } else {
        // overwrite data
        overwriteFileData(position, bytes, replaced)
    }
  }

  private def insertDataInMemory[T <% Traversable[Byte]](position : Long, bytes : T) = {
      for(channel <- seekableChannel(WRITE) ) {
            channel position position
            var buffers = (ByteBuffer allocateDirect MaxPermittedInMemory, ByteBuffer allocateDirect MaxPermittedInMemory)
            
            channel read buffers._1
            buffers._1.flip
            buffers = buffers.swap
            
            channel position position
            writeTo(channel, bytes, bytes.size)
            
            while(channel.position < channel.size - buffers._2.remaining) {
                
                System.out.println("inserting at "+channel.position+" remaining: "+(channel.size - buffers._2.remaining))
                channel read buffers._1
                buffers._1.flip

                channel.write(buffers._2, channel.position - bytes.size)

                buffers = buffers.swap
            }
            
            channel.write(buffers._2)
      }
  }

  /**
   * Create a temporary file to use for performing certain operations.  It should
   * be as efficient as possible to copy from the temporary file to this Seekable and 
   * vice-versa
   */
  protected def tempFile() : Path = Path.createTempFile()

  private def insertDataTmpFile[T <% Traversable[Byte]](position : Long, bytes : T) = {
      val tmp = tempFile()
      
      var i = -1
      
      tmp.ops writeInts (bytesAsInts.asInstanceOf[LongTraversable[Int]] drop position)
      
      for(channel <- seekableChannel(WRITE) ) {
           channel position  position
           writeTo(channel, bytes, -1)
           writeTo(channel, tmp.ops.bytes, tmp.size)
      }
  }
  
  private def overwriteFileData[T <% Traversable[Byte]](position : Long, bytes : T, replaced : Long) = {
      for(channel <- seekableChannel(WRITE) ) {
          channel.position(position)
          val wrote = writeTo(channel,bytes, replaced)
          
          if(replaced > channel.size // need this in the case where replaced == Long.MaxValue
             || position + replaced > channel.size) {
              channel.truncate(channel.position())
          } else if (replaced > wrote) {
              val length = channel.size - position - replaced
              val srcIndex = position + replaced
              val destIndex = position + wrote
              copySlice(channel, srcIndex, destIndex, length.toInt)
              channel.truncate(destIndex+length)
          }
      }
  }

  private def copySlice(channel : FileChannel, srcIndex : Long, destIndex : Long, length : Int) : Unit = {
      val buf = ByteBuffer.allocate(BufferSize.min(length))
      
      def write(done : Int) = {
          if(length < done + BufferSize) buf.limit((length - done).toInt)

          buf.clear()
          val read = channel.read(buf, srcIndex + done)
          buf.flip()
          val written = channel.write(buf, destIndex + done)
      }
      
      (0 to length by BufferSize) foreach write
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
  *          The number of bytes to append negative number to take all
  *          default is to append all
  */
  def append[T <% Traversable[Byte]](bytes: T, take : Long = -1): Unit = {
      for (c <- seekableChannel(APPEND)) writeTo(c, bytes, take)
  }

  private def writeTo[T <% Traversable[Byte]](c : WritableByteChannel, bytes : T, length : Long) : Long = {
      bytes match {
          case array : Array[Byte] =>
            // for performance try to write Arrays directly
              val count = if(length < 0) bytes.size else length.min(bytes.size)
              
              c.write(ByteBuffer.wrap(array, 0, count.toInt))
          case _ =>
              val buf = ByteBuffer.allocateDirect(BufferSize)
              def write[T <% Traversable[Byte]](written : Long, data:T) : Long = {
                  val numBytes = length match {
                      case -1 | -2 => BufferSize
                      case _ => (length - written).min(BufferSize).toInt
                  }
                  
                  System.err.println("writing bytes: "+numBytes)
                  
                  val (toWrite, remaining) = data.splitAt(numBytes)
                  
                  toWrite foreach buf.put
                  buf.flip
                  val currentWrite : Long = c write buf
                  
                  if (remaining.isEmpty) currentWrite
                  else currentWrite + write (written + toWrite.size, remaining)
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
  def chars(implicit codec: Codec): LongTraversable[Char] = (seekableChannel(READ).reader(codec).chars).asInstanceOf[LongTraversable[Char]]  // TODO this is broke
  def bytesAsInts:LongTraversable[Int] = seekableChannel(READ).bytesAsInts
  
  // required method for Output trait
  protected def outputStream = seekableChannel(WRITE_TRUNCATE:_*).outputStream
  
}
