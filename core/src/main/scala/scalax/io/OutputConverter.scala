/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import collection.immutable.{StringOps, WrappedString}
import java.io.OutputStreamWriter
import java.nio.ByteBuffer
import java.nio.channels.{WritableByteChannel, Channels}

/**
 * Functions used by Seekable and Output to either convert data to bytes for writing
 * or write directly to an output stream depending on the requirements of the caller.
 */
trait OutputConverter[-T] extends Function2[WritableByteChannel,T,Unit] {
  /**
   * Write the data to the OutputStream in the most efficient way possible
   *
   * @param out the output object that the converter write to
   * @param data the data to convert for the OutputStream
   */
  def apply(out: WritableByteChannel, data: T):Unit =
    OutputConverter.TraversableByteConverter(out,toBytes(data))

  /**
   * Converts the data to bytes
   * @param data the data to convert to bytes
   * @return The data converted to bytes
   */
  def toBytes(data:T):TraversableOnce[Byte]

  /**
   * The number of bytes the data will be converted to.
   */
  def sizeInBytes : Int
}

/**
 * Contains implicit Converter objects for the compiler to resolve the converters for write operations
 */
object OutputConverter {
  /**
   * Wraps a ByteBuffer to provide access to a ByteBuffer's built-in conversion
   * methods like putShort.
   *
   * This class is to support implementing custom converters
   *
   * @param size the size of the buffer to use.  Typically this is the size of one T converted to bytes
   * @param op a function that writes T to the ByteBuffer that is passed in.
   * @tparam T the type of object this Buffer will convert
   */
  class Buffer[T](size:Int,op:(ByteBuffer,T)=>Unit) extends Iterator[Byte] {
    val converter = ByteBuffer.allocate(size)
    var i = -1

    /**
     * Call to convert the data and place in the buffer
     *
     * @param value the data to convert
     * @return the Buffer with the converted data
     */
    def put(value:T):Buffer[T] = {
      converter.clear
      i = -1;
      op(converter,value)
      this
    }

    def next() = {
      i += 1
      converter.get(i)
    }
    def hasNext = i < size-1
  }
  /**
   * Converts a OutputConverter[Traversable[T]] to a OutputConverter[T].  This
   * class is to support implementing OutputConverters
   *
   * For maximum performance this class is not recommended.  But for maximum productivity
   * it is worth considering.
   *
   * @param base the basis for creating this OutputConverter.  Essentially this class uses base to convert/write a
   *             single T instead of many
   */
  abstract class NonTraversableAdapter[T](base:OutputConverter[TraversableOnce[T]]) extends OutputConverter[T] {
    override def apply(out: WritableByteChannel, data: T) = base(out,List(data))
    def sizeInBytes = base.sizeInBytes
    def toBytes(data: T) = base.toBytes(List(data))
  }
  /**
   * Converts a OutputConverter[Traversable[T]] to a OutputConverter[Array[T]].  This
   * class is to support implementing OutputConverters.
   *
   * For maximum performance this class is not recommended.  But for maximum productivity
   * it is worth considering.
   *
   * @param base the basis for creating this OutputConverter.
   */
  abstract class ArrayAdapter[T](base:OutputConverter[TraversableOnce[T]]) extends OutputConverter[Array[T]] {
    override def apply(out: WritableByteChannel, data: Array[T]) = base(out,data)
    def sizeInBytes = base.sizeInBytes
    def toBytes(data: Array[T]) = base.toBytes(data)
  }
  implicit object ByteBlockConverter extends OutputConverter[ByteBlock] {
    def sizeInBytes = 1
    def toBytes(data: ByteBlock):TraversableOnce[Byte] = data.toIterator
  }
  implicit object ByteConverter extends NonTraversableAdapter(TraversableByteConverter)
  implicit object ByteArrayConverter extends ArrayAdapter(TraversableByteConverter) {
    override def apply(out: WritableByteChannel, bytes:Array[Byte]) = if(bytes.length > 0) out.write(ByteBuffer.wrap(bytes))
  }
  implicit object ByteBufferConverter extends OutputConverter[ByteBuffer] {
    override def apply(out: WritableByteChannel, bytes:ByteBuffer) = out.write(bytes)
      def toBytes(data: ByteBuffer) = new nio.ByteBuffer(data)
      def sizeInBytes = 1    
  }
  implicit object TraversableByteConverter extends OutputConverter[TraversableOnce[Byte]] {
    override def apply(out: WritableByteChannel, bytes:TraversableOnce[Byte]) = {
      if(bytes.hasDefiniteSize && bytes.size < 8*1024) {
        out write  ByteBuffer.wrap (bytes.toArray)
      } else {
        val buffer = ByteBuffer.allocate(8*1024)
        bytes foreach {
          case i if buffer.position() < buffer.capacity() =>
            buffer.put(i)
          case i => 
            buffer.flip()
            out write buffer
            buffer.clear()
            buffer.put(i)
        }
        if (buffer.remaining() > 0) {
          buffer.flip()
          out write buffer
        }
      }
    }
    def toBytes(data: scala.TraversableOnce[Byte]) = data
    def sizeInBytes = 1
  }
  implicit object BooleanConverter extends NonTraversableAdapter(TraversableBooleanConverter)
  implicit object BooleanArrayConverter extends ArrayAdapter(TraversableBooleanConverter)
  implicit object TraversableBooleanConverter extends OutputConverter[TraversableOnce[Boolean]] {
    def toBytes(data: scala.TraversableOnce[Boolean]) = data.toIterator.map{b => if(b) 1:Byte else 0:Byte}
    def sizeInBytes = 1
  }
  implicit object ShortConverter extends NonTraversableAdapter(TraversableShortConverter)
  implicit object ShortArrayConverter extends ArrayAdapter(TraversableShortConverter)
  implicit object TraversableShortConverter extends OutputConverter[TraversableOnce[Short]] {
    def toBytes(data: scala.TraversableOnce[Short]) = {
      val buffer = new Buffer[Short](sizeInBytes, (buf,v) => buf.putShort(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 8
  }
  implicit object LongConverter extends NonTraversableAdapter(TraversableLongConverter)
  implicit object LongArrayConverter extends ArrayAdapter(TraversableLongConverter)
  implicit object TraversableLongConverter extends OutputConverter[TraversableOnce[Long]] {
    def toBytes(data: scala.TraversableOnce[Long]) = {
      val buffer = new Buffer[Long](sizeInBytes, (buf,v) => buf.putLong(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 8
  }
  implicit object DoubleConverter extends NonTraversableAdapter(TraversableDoubleConverter)
  implicit object DoubleArrayConverter extends ArrayAdapter(TraversableDoubleConverter)
  implicit object TraversableDoubleConverter extends OutputConverter[TraversableOnce[Double]] {
    def toBytes(data: scala.TraversableOnce[Double]) = {
      val buffer = new Buffer[Double](sizeInBytes, (buf,v) => buf.putDouble(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 8
  }
  implicit object FloatConverter extends NonTraversableAdapter(TraversableFloatConverter)
  implicit object FloatArrayConverter extends ArrayAdapter(TraversableFloatConverter)
  implicit object TraversableFloatConverter extends OutputConverter[TraversableOnce[Float]] {
    def toBytes(data: scala.TraversableOnce[Float]) = {
      val buffer = new Buffer[Float](sizeInBytes, (buf,v) => buf.putFloat(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 4
  }
  implicit object IntConverter extends NonTraversableAdapter(TraversableIntConverter)
  implicit object IntArrayConverter extends ArrayAdapter(TraversableIntConverter)
  implicit object TraversableIntConverter extends OutputConverter[TraversableOnce[Int]] {
    def toBytes(data: scala.TraversableOnce[Int]) = {
      val buffer = new Buffer[Int](sizeInBytes, (buf,v) => buf.putInt(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 4
  }
  object IntAsByteConverter extends NonTraversableAdapter(TraversableIntAsByteConverter)
  object IntAsByteArrayConverter extends ArrayAdapter(TraversableIntAsByteConverter)
  object TraversableIntAsByteConverter extends OutputConverter[TraversableOnce[Int]] {
    def toBytes(data: scala.TraversableOnce[Int]) = data.toIterator.map{_.toByte}
    def sizeInBytes = 1
  }
  private class CharConverter(codec : Codec) extends NonTraversableAdapter(new TraversableCharConverter(codec))
  private class TraversableCharConverter(codec : Codec) extends OutputConverter[TraversableOnce[Char]] {
    override def apply(out: WritableByteChannel, characters:TraversableOnce[Char]) = {
      val writer = new OutputStreamWriter(Channels.newOutputStream(out))  // use nio writing soon
      try {
        characters match {
          case string : StringOps => writer write string
          case string : WrappedString => writer write string
          case _ => characters foreach writer.append
        }
      }finally {
        writer.flush();
      }
    }

    def toBytes(data: scala.TraversableOnce[Char]) = data.toIterator.flatMap{c => codec.encode(c.toString).toIterator}
    def sizeInBytes = codec.encoder.maxBytesPerChar.toInt
  }

  implicit def charsToOutputFunction(implicit codec:Codec = Codec.default):OutputConverter[TraversableOnce[Char]] = new TraversableCharConverter(codec)
  implicit def charToOutputFunction(implicit codec:Codec = Codec.default):OutputConverter[Char] = new CharConverter(codec)
}
