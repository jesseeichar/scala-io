/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import collection.immutable.{StringOps, WrappedString}
import java.io.{OutputStream, OutputStreamWriter}
import java.nio.ByteBuffer

/**
 * Functions used by Seekable and Output to either convert data to bytes for writing
 * or write directly to an output stream depending on the requirements of the caller.
 *
 * User: jeichar
 * Date: Sep 14, 2010
 * Time: 9:27:50 PM
 */

trait OutputConverter[-T] extends Function2[OutputStream,T,Unit] {
  /**
   * Convert to bytes.
   */
  def toBytes(data:T):TraversableOnce[Byte]
  def sizeInBytes : Int
}

object OutputConverter {
  private class Buffer[T](size:Int,op:(ByteBuffer,T)=>Unit) extends Iterator[Byte] {
    val converter = ByteBuffer.allocate(size)
    var i = -1

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
  abstract class NonTraversableAdapter[T](base:OutputConverter[TraversableOnce[T]]) extends OutputConverter[T] {
    def apply(out: OutputStream, data: T) = base(out,List(data))
    def sizeInBytes = base.sizeInBytes
    def toBytes(data: T) = base.toBytes(List(data))
  }
  abstract class ArrayAdapter[T](base:OutputConverter[TraversableOnce[T]]) extends OutputConverter[Array[T]] {
    def apply(out: OutputStream, data: Array[T]) = base(out,data)
    def sizeInBytes = base.sizeInBytes
    def toBytes(data: Array[T]) = base.toBytes(data)
  }
  implicit object ByteConverter extends NonTraversableAdapter(TraversableByteConverter)
  implicit object ByteArrayConverter extends ArrayAdapter(TraversableByteConverter)
  implicit object TraversableByteConverter extends OutputConverter[TraversableOnce[Byte]] {
    def apply(out: OutputStream, bytes:TraversableOnce[Byte]) = {
      bytes foreach {i => out write i.toInt}
    }
    def toBytes(data: scala.TraversableOnce[Byte]) = data
    def sizeInBytes = 1
  }
  implicit object ShortConverter extends NonTraversableAdapter(TraversableShortConverter)
  implicit object ShortArrayConverter extends ArrayAdapter(TraversableShortConverter)
  implicit object TraversableShortConverter extends OutputConverter[TraversableOnce[Short]] {
    def apply(out: OutputStream, shorts:TraversableOnce[Short]) = TraversableByteConverter(out,toBytes(shorts))
    def toBytes(data: scala.TraversableOnce[Short]) = {
      val buffer = new Buffer[Short](sizeInBytes, (buf,v) => buf.putShort(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 8
  }
  implicit object LongConverter extends NonTraversableAdapter(TraversableLongConverter)
  implicit object LongArrayConverter extends ArrayAdapter(TraversableLongConverter)
  implicit object TraversableLongConverter extends OutputConverter[TraversableOnce[Long]] {
    def apply(out: OutputStream, longs:TraversableOnce[Long]) = TraversableByteConverter(out,toBytes(longs))
    def toBytes(data: scala.TraversableOnce[Long]) = {
      val buffer = new Buffer[Long](sizeInBytes, (buf,v) => buf.putLong(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 8
  }
  implicit object DoubleConverter extends NonTraversableAdapter(TraversableDoubleConverter)
  implicit object DoubleArrayConverter extends ArrayAdapter(TraversableDoubleConverter)
  implicit object TraversableDoubleConverter extends OutputConverter[TraversableOnce[Double]] {
    def apply(out: OutputStream, doubles:TraversableOnce[Double]) = TraversableByteConverter(out,toBytes(doubles))
    def toBytes(data: scala.TraversableOnce[Double]) = {
      val buffer = new Buffer[Double](sizeInBytes, (buf,v) => buf.putDouble(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 8
  }
  implicit object FloatConverter extends NonTraversableAdapter(TraversableFloatConverter)
  implicit object FloatArrayConverter extends ArrayAdapter(TraversableFloatConverter)
  implicit object TraversableFloatConverter extends OutputConverter[TraversableOnce[Float]] {
    def apply(out: OutputStream, floats:TraversableOnce[Float]) = TraversableByteConverter(out,toBytes(floats))
    def toBytes(data: scala.TraversableOnce[Float]) = {
      val buffer = new Buffer[Float](sizeInBytes, (buf,v) => buf.putFloat(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 4
  }
  implicit object IntConverter extends NonTraversableAdapter(TraversableIntConverter)
  implicit object IntArrayConverter extends ArrayAdapter(TraversableIntConverter)
  implicit object TraversableIntConverter extends OutputConverter[TraversableOnce[Int]] {
    def apply(out: OutputStream, integers:TraversableOnce[Int]) = TraversableByteConverter(out,toBytes(integers))
    def toBytes(data: scala.TraversableOnce[Int]) = {
      val buffer = new Buffer[Int](sizeInBytes, (buf,v) => buf.putInt(v))
      data.toIterator.flatMap{buffer.put _}
    }
    def sizeInBytes = 4
  }
  object IntAsByteConverter extends NonTraversableAdapter(TraversableIntAsByteConverter)
  object IntAsByteArrayConverter extends ArrayAdapter(TraversableIntAsByteConverter)
  object TraversableIntAsByteConverter extends OutputConverter[TraversableOnce[Int]] {
    def apply(out: OutputStream, integers:TraversableOnce[Int]) = integers foreach out.write
    def toBytes(data: scala.TraversableOnce[Int]) = data.toIterator.map{_.toByte}
    def sizeInBytes = 1
  }
  private class CharConverter(codec : Codec) extends NonTraversableAdapter(new TraversableCharConverter(codec))
  private class TraversableCharConverter(codec : Codec) extends OutputConverter[TraversableOnce[Char]] {
    def apply(out: OutputStream, characters:TraversableOnce[Char]) = {
      val writer = new OutputStreamWriter(out)
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

  implicit def charsToOutputFunction(implicit codec:Codec):OutputConverter[TraversableOnce[Char]] = new TraversableCharConverter(codec)
  implicit def charToOutputFunction(implicit codec:Codec):OutputConverter[Char] = new CharConverter(codec)
}
