/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{ BufferedInputStream, InputStream, PrintStream, File => JFile }
import java.io.{ BufferedReader, InputStreamReader }
import java.net.{ URI, URL }

import collection.mutable.ArrayBuffer
import Path.fail

/**
 * Traits which can be viewed as a sequence of bytes.  Source types
 * which know their length should override def length: Long for more
 * efficient method implementations.
 * <p>
 * The default implementation uses the inputStream 
 * basis for reading the data.
 * </p>
 */
trait Bytes {
  /**
   * Obtains an {@InputStreamResource} for input
   * operations
   */
  def inputStream: InputStreamResource

  /**
   * The number of bytes available for reading
   * <p>
   * if length == -1 then it is not possible to determine the
   * number of bytes in advance.
   * </p>
   */
  def length: Long = -1

  /**
   * Obtains a Iterable for conveniently processing the resource as bytes.
   * <p>
   * Depending on the underlying resource this may be slower than
   * {@link #bytesAsInts}
   * </p>
   */
  def bytes(): Iterable[Byte] = bytesAsInts() map (_.toByte)
  /**
   * Obtains a Iterable for conveniently processing the file as Ints.
   * <p>
   * Depending on the underlying resource this may be slower than
   * {@link #bytes}
   * </p>
   * <p>
   * This is a View so remember to treat it as a view and not as a Stream or
   * a strict collection
   */
  def bytesAsInts(): Iterable[Int] = {
    val resource = inputStream.buffered
    resource.toTraversable( in => {
      Iterator continually in.read() takeWhile (_ != -1)
    }).view.toIterable
  }

  /**
   * This method aspires to be the fastest way to read
   * a stream of known length into memory.
   */
  def slurpBytes(): Array[Byte] = {
    // if we don't know the length, fall back on relative inefficiency
    if (length == -1L)
      return (new ArrayBuffer[Byte]() ++= bytes()).toArray

    val arr = new Array[Byte](length.toInt)
    val len = arr.length
    lazy val resource = inputStream.buffered
    var offset = 0

    def loop(in:InputStream) {
      if (offset < len) {
        val read = in.read(arr, offset, len - offset)
        if (read >= 0) {
          offset += read
          loop(in)
        }
      }
    }
    resource acquireFor (loop _)

    if (offset == arr.length) arr
    else fail("Could not read entire source (%d of %d bytes)".format(offset, len))
  }
}

/**
 * For objects which can be viewed as Chars.  The abstract creationCodec
 * can safely be defined as null and will subsequently be ignored.
 */
trait Chars extends Bytes {
  def creationCodec: Codec
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
    else if (creationCodec != null) creationCodec
    else if (allowDefault) Codec.default
    else failNoCodec()

  def chars(codec: Codec = getCodec()): Iterable[Char] = bytesAsInts() map (c => (codec wrap c).toChar)
  /**
   * terminator is 1-2 characters
   */
  def lines(terminator: String = compat.Platform.EOL,
            includeTerminator: Boolean = false,
            codec: Codec = getCodec()): Iterable[String] =
              {
                require(terminator.length == 1 || terminator.length == 2, "Line terminator may be 1 or 2 characters only.")
                new Iterable[String] {
                  def iterator = new LineIterator(chars(codec), terminator, includeTerminator)
                }
              }
  /**
   * Convenience function to import entire file into a String.
   */
  def slurpString(codec: Codec = getCodec()) = chars(codec).mkString

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
  }
}

