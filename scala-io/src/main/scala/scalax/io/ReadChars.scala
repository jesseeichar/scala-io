/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.io.resource._
import scala.collection.Traversable
import OpenOption._
import Line._
import java.io.{ 
  InputStream, PrintStream, File => JFile,
  InputStreamReader, OutputStream, Writer, Reader
}
import java.nio.channels.{
  ByteChannel, WritableByteChannel, FileChannel, ReadableByteChannel,
  Channels
}
import java.net.{ URI, URL }

import collection.mutable.ArrayBuffer
import Path.fail

/**
 * An trait for objects that viewed as a sequence of bytes. For example InputStream
 * and ReadableByteChannel could be an Input object (or be converted
 * to a ReadBytes object).
 * <p>
 * Note: All collections returned are non-strict collections and each
 * invocation of a method will typically open a new stream or channel.
 * That behaviour can be overrided by the implementation but
 * it is the default behaviour.
 * </p>
 * <p>
 * Default implementation is based on providing an implementation for 
 * bytesAsInts and all other methods are implemented using
 * that method.  
 * <p>
 * An alternate way to provide an implementation is to override chars and implement bytesAsInts 
 * by calling charsToInts
 * </p>
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see Output
 */
trait ReadChars {
  /**
   * The characters in the object.
   * <p>
   * If the codec is not the same as the source codec (the codec of
   * the underlying data) then the characters will converted to the
   * desired codec.
   * </p><p>
   * In some object the bytes of underlying iterable can be cast to a Seq
   * and elements can be randomly accessed. Random access must be used
   * carefully as each access will open a new stream unless that behavior
   * is modified by the implementation.
   * </p><p>
   * For example on some filesystems using random access within a
   * {@link FileOperations#open} will perform all accesses using
   * the same Channel improving the performance.
   * </p>
   * 
   * @param codec
   *          The codec representing the desired encoding of the characters
   * @return
   *          an iterable of all the characters
   */
  def chars: Traversable[Char]
  /**
   * Obtain an non-strict iterable for iterating through the lines in the object
   * <p>
   * If the codec is not the same as the source codec (the codec of
   * the underlying data) then the characters will converted to the
   * desired codec.
   * </p><p>
   * In some object the bytes of underlying iterable can be cast to a Seq
   * and elements can be randomly accessed. Random access must be used
   * carefully as each access will open a new stream unless that behavior
   * is modified by the implementation.
   * </p><p>
   * For example on some filesystems using random access within a
   * {@link FileOperations#open} will perform all accesses using
   * the same Channel improving the performance.
   * </p>       
   *
   * @param codec
   *          The codec representing the desired encoding of the characters
   * @param terminator
   *          The strategy for determining the end of line
   *          Default is to auto-detect the EOL
   * @param includeTerminator
   *          if true then the line will end with the line terminator
   *          Default is false
   *
   * @return
   *          a non-strict iterable for iterating through all the lines
   */
  def lines(terminator: Terminators.Terminator = new Terminators.Auto(),
            includeTerminator: Boolean = false): Traversable[String] = {
             new LineTraverseable(chars, terminator, includeTerminator)
        }
  /**
   * Loads all the characters into memory. There is no protection against
   * loading very large files/amounts of data.
   * <p>
   * If the codec is not the same as the source codec (the codec of
   * the underlying data) then the characters will converted to the
   * desired codec.
   * </p>
   * @param codec
   *          The codec representing the desired encoding of the characters  
   */
  def slurpString = chars.mkString

}