/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
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
 * For objects which can be viewed as Chars.  The abstract sourceCodec
 * can safely be defined as null and will subsequently be ignored.
 *
 * @author Jesse Eichar
 * @since 1.0
 * 
 * @see Bytes
 * @see ReadBytes
 * @see WriteBytes
 * @see ReadChars
 * @see WriteChars
 */
trait Chars {
  /**
   * The codec representing encoding of the underlying data
   */
  protected def sourceCodec: Codec
  private def failNoCodec() = fail("This method requires a Codec to be chosen explicitly.")

  /**
   * The general algorithm for any call to a method involving byte<->char
   * transformations is: if a codec is supplied (explicitly or implicitly),
   * use that; otherwise if a codec was defined when the object was created,
   * use that; otherwise, use Codec.default.
   *
   * Note that getCodec takes a codec argument rather than having methods
   * always default to getCodec() and use the argument otherwise, so that
   * method implementations can, where desired, identify the case where no
   * codec was ever explicitly supplied.  If allowDefault = false, an
   * exception will be thrown rather than falling back on Codec.default.
   */
  def getCodec(givenCodec: Codec = null, allowDefault: Boolean = true) =
    if (givenCodec != null) givenCodec
    else if (sourceCodec != null) sourceCodec
    else if (allowDefault) Codec.default
    else failNoCodec()


  /**
   * Creates a new BasicFileOperations object with
   * the new codec
   */
  def withCodec(codec:Codec): Chars
}

/**
 * An trait for objects that viewed as a sequence of Chars. For example InputStream,
 * Reader and ReadableByteChannel could all be a ReadChars object (or be converted
 * to a ReadChars object).
 * <p>
 * Note: All collections returned are non-strict collections and each
 * invocation of a method will typically open a new stream or channel.
 * That behaviour can be overrided by the implementation but
 * it is the default behaviour.
 * </p>
 *
 * @author Jesse Eichar
 * @since 1.0
 * 
 * @see Bytes
 * @see ReadBytes
 * @see WriteBytes
 * @see Chars
 * @see WriteChars
 */
trait ReadChars extends Chars {
  def withCodec(codec:Codec): ReadChars

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
  def chars(codec: Codec = getCodec()): Iterable[Char] = null // TODO bytesAsInts() map (c => (codec wrap c).toChar)
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
  def lines(terminator: Terminators.Terminator = Terminators.Auto,
            includeTerminator: Boolean = false,
            codec: Codec = getCodec()): Iterable[String] = {
             /* require(terminator.length == 1 || terminator.length == 2, "Line terminator may be 1 or 2 characters only.")
              new Iterable[String] {
                def iterator = new LineIterator(chars(codec), terminator, includeTerminator)
              }*/
              null //TODO
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
  def slurpString(codec: Codec = getCodec()) = chars(codec).mkString

/*
 * TODO convert to use Line.Terminator for finding new line
  private class LineIterator(source: Iterable[Char], terminator: String, includeTerminator: Boolean) extends Iterator[String] {
    require(terminator.length == 1 || terminator.length == 2, "Line terminator may be 1 or 2 characters only.")

    lazy val iter = source.iterator.buffered
    // For two character newline sequences like \r\n, we peek at
    // the iterator head after seeing \r, and drop the \n if present.
    val isNewline: Char => Boolean = {
      val firstCh = terminator(0)
      if (terminator.length == 1) (_ == firstCh)
      else (ch: Char) => (ch == firstCh) && iter.hasNext && {
        val res = iter.head == terminator(1)
        if (res) { iter.next }  // drop the second character
        res
      }
    }
    private[this] val sb = new StringBuilder

    private def getc() =
      if (!iter.hasNext) false
      else {
        val ch = iter.next
        if (isNewline(ch)) {
          if (includeTerminator) sb append ch
          false
        } else {
          sb append ch
          true
        }
      }

    def hasNext = iter.hasNext
    def next = {
      sb.clear
      while (getc()) { }
      sb.toString
    }
  } */
}

/**
 * A trait for objects that have bytes written to them. For example an
 * OutputStream and File can both be WriteChars (or be converted to one).
 * Depending on the implementation and the underlying object the
 * {@link OpenOption} may be restricted to a subset of the
 * {@link OpenOption}
 * 
 * <p>
 * Note: Each invocation of a method will typically open a new stream or
 * channel.  That behaviour can be overrided by the implementation but
 * it is the default behaviour.
 * </p>
 *
 * @author Jesse Eichar
 * @since 1.0
 * 
 * @see Bytes
 * @see ReadBytes
 * @see WriteBytes
 * @see Chars
 * @see ReadChars
 */
trait WriteChars extends Chars {
  def withCodec(codec:Codec): WriteChars

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
   * @param openOptions
   *          the options to use when preparing to write. The implementation
   *          must declare which options can be used.
   *          Default is standard options write/create/truncate
   */
  def writeString(string: String,
                  codec: Codec = getCodec(),
                  openOptions: Iterable[OpenOption] = WRITE_TRUNCATE): Unit = {
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
   * @param codec
   *          The codec of the strings to be written. The strings will
   *          be converted to the encoding of {@link sourceCodec}
   *          Default is sourceCodec
   * @param openOptions
   *          The options to use when preparing to write. The implementation
   *          must declare which options can be used.
   *          Default is standard options write/create/truncate
   */  
  def writeStrings(strings: Traversable[String],
                   codec: Codec = getCodec(),
                   openOptions: Iterable[OpenOption] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
  }

  /**
   * Writes several strings to file adding a separator between each string.
   * The open options that can be used are dependent on the implementation
   * and implementors should clearly document which option are permitted.
   * 
   * @param lines
   *          The data to write
   * @param terminator
   *          The End of Line character or line terminator
   *          Default is Line.Terminators.NewLine
   * @param codec
   *          The codec of the string to be written.
   *          The string will be converted to the encoding of {@link sourceCodec}
   *          Default is sourceCodec
   * @param openOptions
   *          The options to use when preparing to write.
   *          The implementation must declare which options can be used.
   *          Default is standard options write/create/truncate
   */
  def writeLines(strings: Traversable[String],
                 terminator: Terminators.Terminator = Terminators.NewLine,
                 codec: Codec = getCodec(),
                 openOptions: Iterable[OpenOption] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
  }

}
