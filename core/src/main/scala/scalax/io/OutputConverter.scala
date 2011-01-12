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

/**
 * Functions used by Seekable and Output to either convert data to bytes for writing
 * or write directly to an output stream depending on the requirements of the caller.
 *
 * User: jeichar
 * Date: Sep 14, 2010
 * Time: 9:27:50 PM
 */

trait OutputConverter[T] extends Function2[OutputStream,TraversableOnce[T],Unit] {
  /**
   * Convert to bytes.
   */
  def toBytes(data:TraversableOnce[T]):TraversableOnce[Byte]
  def sizeInBytes : Int
}

object OutputConverter {
  implicit object ByteConverter extends OutputConverter[Byte] {
    def apply(out: OutputStream, bytes:TraversableOnce[Byte]) = {
      bytes foreach {i => out write i.toInt}
    }
    def toBytes(data: scala.TraversableOnce[Byte]) = data
    def sizeInBytes = 1
  }
  implicit object IntConverter extends OutputConverter[Int] {
    def apply(out: OutputStream, integers:TraversableOnce[Int]) = {
      integers foreach out.write
    }
    // TODO is this what I want? or do I want to encode as 2 bytes?
    def toBytes(data: scala.TraversableOnce[Int]) = data.toIterator.map{_.toByte}
    def sizeInBytes = 1
  }
  private class CharConverter(implicit codec : Codec) extends OutputConverter[Char] {
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

  implicit def charsToOutputFunction[T](implicit codec:Codec):OutputConverter[Char] = new CharConverter
}
