/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{
    InputStream, OutputStream
}
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.channels.{
    ByteChannel, WritableByteChannel
}
import scalax.io.resource._
import scala.collection.Traversable
import OpenOption._
import Resource._
import scala.annotation._
/**
 * An object for reading and writing to Random Access IO objects such as Files.
 *
 * @author Jesse Eichar
 * @since 1.0
 */
trait Seekable extends Input with Output {
    private final val BufferSize=8192
    private final val MaxPermittedInMemory=8192
    
    private final val Overwrite = Long.MinValue
    

    // for Java 7 change this to a Seekable Channel
    /**
     * The underlying channel to write to.  The open options indicate the preferred way
     * to interact with the underlying channel.
     */
    protected def channel(openOptions:OpenOption*) : OutputResource[SeekableByteChannel] with InputResource[SeekableByteChannel]

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
   *          The position is the position'th character in the file using
   *          the codec for decoding the file
   *          The position must be within the file.
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
                  replaced : Long = Overwrite)(implicit codec: Codec): Unit = {
    require(position >= 0, "The patch starting position must be greater than or equal 0")

    if(size.forall{position > _}){
      // special case where there is no alternative but to be append
      append(string getBytes codec.name)
    } else if (codec.hasConstantSize) {
      // special case where the codec is constant in size (like ASCII or latin1)
      val bytesPerChar = codec.encoder.maxBytesPerChar.toLong
      val posInBytes = if(position > 0) position * bytesPerChar else position
      val replacedInBytes = if(replaced > 0) replaced * bytesPerChar else replaced
      
      patch(posInBytes, string.getBytes(codec.name), replacedInBytes)
    } else {
      // when a charset is not constant (like UTF-8 or UTF-16) the only
      // way is to find position is to iterate to the position counting characters
      // Same with figuring out what replaced is in bytes

      // this is very inefficient.  The file is opened 3 times.
      val posInBytes = charCountToByteCount(0, position)
      val replacedInBytes = charCountToByteCount(position, position+replaced)
//      println("replacedInBytes",replacedInBytes)
      patch(posInBytes, string.getBytes(codec.name), replaced max replacedInBytes)
    }
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
            replaced : Long = Overwrite): Unit = {
    require(position >= 0, "The patch starting position must be greater than or equal 0")

    val appendData = size.forall{position == _}
    val insertData = replaced <= 0 && replaced != Overwrite
        
    if(appendData) {
      append(bytes, replaced)
    } else if(insertData) {
      insert(position : Long, bytes : T)
    } else {
      // overwrite data
      overwriteFileData(position, bytes, replaced)
    }
  }
  
  private def insert[T <% Traversable[Byte]](position : Long, bytes : T) = {
    if (bytes.hasDefiniteSize && bytes.size <= MaxPermittedInMemory) {
        insertDataInMemory(position, bytes)
    } else {
        insertDataTmpFile(position, bytes)
    }
  }

  private def insertDataInMemory[T <% Traversable[Byte]](position : Long, bytes : T) = {
      for(channel <- channel(Write) ) {
            channel position position
            var buffers = (ByteBuffer allocateDirect MaxPermittedInMemory, ByteBuffer allocateDirect MaxPermittedInMemory)
            
            channel read buffers._1
            buffers._1.flip
            buffers = buffers.swap
            
            channel position position
            writeTo(channel, bytes, bytes.size)
            
            while(channel.position < channel.size - buffers._2.remaining) {
                
//                System.out.println("inserting at "+channel.position+" remaining: "+(channel.size - buffers._2.remaining))
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
      
      tmp writeInts (bytesAsInts.asInstanceOf[LongTraversable[Int]] ldrop position)
      
      for(channel <- channel(Write) ) {
           channel position  position
           writeTo(channel, bytes, -1)
           writeTo(channel, tmp.bytes, tmp.size.get)
      }
  }
  
  private def overwriteFileData[T <% Traversable[Byte]](position : Long, bytes : T, replaced : Long) = {
      for(channel <- channel(Write) ) {
          channel.position(position)
//            println("byte size,replaced",bytes.size,replaced)
          val (wrote, earlyTermination) = writeTo(channel,bytes, replaced)
          
//          println("wrote,earlyTermination",wrote,earlyTermination)

          if(replaced > channel.size // need this in the case where replaced == Long.MaxValue
             || position + replaced > channel.size) {
              channel.truncate(channel.position)
          } else if (replaced > wrote) {
              val length = channel.size - position - replaced
              val srcIndex = position + replaced
              val destIndex = position + wrote
              copySlice(channel, srcIndex, destIndex, length.toInt)
              channel.truncate(destIndex+length)
          } else if (earlyTermination) {
            val adjustedPosition = position +  replaced
            bytes match {
              case b : LongTraversable[_] => insert(adjustedPosition,b.asInstanceOf[LongTraversable[Byte]] ldrop wrote)
              case _ => insert(adjustedPosition,bytes drop wrote.toInt)
            }
          }
      }
  }

  private def copySlice(channel : SeekableByteChannel, srcIndex : Long, destIndex : Long, length : Int) : Unit = {

//      println("copySlice(srcIndex, destIndex, length)", srcIndex, destIndex, length)

      val buf = ByteBuffer.allocate(BufferSize.min(length))
      def write(done : Int) = {
//        println("copySlice:write(done)", done)
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
      for (c <- channel(Append)) writeTo(c, bytes, take)
  }

  // returns (wrote,earlyTermination)
  private def writeTo[T <% Traversable[Byte]](c : WritableByteChannel, bytes : T, length : Long) : (Long,Boolean) = {
//    println("start write",length)
      bytes match {
          case array : Array[Byte] =>
            // for performance try to write Arrays directly
              val count = if(length < 0) bytes.size else length.min(bytes.size)
              val wrote = c.write(ByteBuffer.wrap(array, 0, count.toInt))

//              println("count,bytes.size,wrote,length",count,bytes.size,wrote,length)
              
              val isWriteAll = length > 0
              (wrote, isWriteAll && length < bytes.size)
          case _ =>
            // TODO user hasDefinitateSize to improve performance
            // if the size is small enough we can probably open a memory mapped buffer
            // or at least copy to a buffer in one go.
              val buf = ByteBuffer.allocateDirect(BufferSize)
              var earlyTermination = false
              
// TODO add this back when I can get tailrec working              
//              @tailrec
              def write[T <% Traversable[Byte]](written : Long, data:T) : Long = {
                  val numBytes = length match {
                      case -1 | Overwrite => BufferSize
                      case _ => (length - written).min(BufferSize).toInt
                  }
                  
//                  println("writing bytes: "+numBytes)
                  
                  val (toWrite, remaining) = data.splitAt(numBytes)
                  
                  assert(numBytes > 0 || remaining.nonEmpty)                  
                  
                  toWrite foreach buf.put
                  buf.flip
                  val currentWrite : Long = c write buf
                  earlyTermination = length <= written + numBytes && remaining.nonEmpty
                  
                  if (earlyTermination || remaining.isEmpty) currentWrite
                  else currentWrite + write (written + numBytes, remaining)
              }
              
             (write(0, bytes), earlyTermination)
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
      append(codec encode string)
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
    val sepBytes = codec encode separator
    for (c <- channel(Append)) (strings foldLeft false){ 
      (addSep, string) =>
        if(addSep) writeTo(c, sepBytes, Long.MaxValue)
        writeTo(c, codec encode string, Long.MaxValue)
        
        true
    }
  }
  
  def chop(position : Long) : Unit = {
       channel(Append) foreach {_.truncate(position)}
  }
  
  def chopString(position : Long)(implicit codec:Codec) : Unit = {
    val posInBytes = charCountToByteCount(0,position)
    channel(Append) foreach {_.truncate(posInBytes)}
  }
  
  // required methods for Input trait
  def chars(implicit codec: Codec): ResourceView[Char] = (channel(Read).reader(codec).chars).asInstanceOf[ResourceView[Char]]  // TODO this is broke
  def bytesAsInts:ResourceView[Int] = channel(Read).bytesAsInts
  
  // required method for Output trait
  protected def outputStream = channel(WriteTruncate:_*).outputStream

  private def charCountToByteCount(start:Long, end:Long)(implicit codec:Codec) = {
    val encoder = codec.encoder
    val byteBuffer = ByteBuffer.allocateDirect(encoder.maxBytesPerChar.toInt)
    val charBuffer = CharBuffer.allocate(1)

    def sizeInBytes(c : Char) = {
      c.toString.getBytes(codec.name).size // this is very inefficient

      /* TODO There is a bug in this implementation when encoding certain characters like \n

      encoder.reset
      byteBuffer.clear()
      charBuffer.put(0,c)
      charBuffer.position(0)
      val result = encoder.encode(charBuffer, byteBuffer, true)

      assert(!result.isUnderflow, "Attempted to encode "+c+" in charset "+codec.name+" but got an underflow error")
      assert(!result.isOverflow, "Attempted to encode "+c+" in charset "+codec.name+" but got an overflow error")
      assert(!result.isError, "Attempted to encode "+c+" in charset "+codec.name+" but got an error")

      println("sizeInBytes of '"+c+"' is "+byteBuffer.position)

      byteBuffer.position
      */
    }

    val segment = channel(Read).chars.lslice(start, end)
    
    (0L /: segment ) { (replacedInBytes, nextChar) => 
          replacedInBytes + sizeInBytes(nextChar)
    }    
  }
}
