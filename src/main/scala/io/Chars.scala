/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection.Traversable
import StandardOpenOptions._

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
 * Factory for creating {@link ReadChars} and {@link WriteChars} from
 * streams, readers, writers and channels.
 * <p>
 * Note: It is highly recommended to pass the stream/channel creation code to the factory method
 * so that the Read/WriteChars object can be reused.  For example:
 * <pre><code>
 * val url = new URL("www.scala-lang.org")
 * Chars.fromInputStream(url.openStream)
 * </code></pre>
 * is preferable to
 * <pre><code>
 * val stream = new URL("www.scala-lang.org")
 * Chars.fromInputStream(stream)
 * </code></pre>
 * The ReadChars obtained from latter example can only be
 * used once before the stream is used.  But the former
 * can be re-used because the parameter is the function required
 * to create the input stream
 */
object Chars {
  def fromInputStream(inputStream: => InputStream): ReadChars with ReadBytes = fromReadableByteChannel(Channels.newChannel(inputStream)) 
  def fromReadableByteChannel(channel: => ReadableByteChannel): ReadChars with ReadBytes =  new Read(channel)
  def fromReader(reader: => Reader): ReadChars = null // TODO implement

  def fromOutputStream(outputStream: => OutputStream): WriteChars = fromWritableByteChannel(Channels.newChannel(outputStream)) 
  def fromWritableByteChannel(channel: => WritableByteChannel): WriteChars with WriteBytes =  new Write(channel)
  def fromWriter(reader: => Writer): WriteChars = null // TODO implement

  def fromByteChannel(channel: => ByteChannel): ReadChars with WriteChars with ReadBytes with WriteBytes =  new ReadWrite(channel)
  def fromFileChannel(channel: => FileChannel): ReadChars with WriteChars with ReadBytes with WriteBytes =  new ReadWrite(channel)

  private class Read (channel: => ReadableByteChannel) extends ReadChars with ReadBytes{
    protected lazy val obtainReadableByteChannel = IoResource.fromReadableByteChannel (channel)
    def sourceCodec: Codec = Codec.default
  }
  private class Write (channel: => WritableByteChannel) extends WriteChars with WriteBytes{
    protected lazy val obtainWritableByteChannel = IoResource.fromWritableByteChannel (channel)
    def sourceCodec: Codec = Codec.default
  }
  private class ReadWrite (channel: => ByteChannel) extends ReadChars with WriteChars with ReadBytes with WriteBytes{
    lazy val resource = IoResource.fromByteChannel (channel)
    protected lazy val obtainWritableByteChannel = resource
    protected lazy val obtainReadableByteChannel = resource
    def sourceCodec: Codec = Codec.default
  }
}
 

/**
 * For objects which can be viewed as Chars.  The abstract sourceCodec
 * can safely be defined as null and will subsequently be ignored.
 */
trait Chars {
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
}
trait ReadChars extends Chars {

  def bytes(): Iterable[Byte]
  def chars(codec: Codec = getCodec()): Iterable[Char] = null // TODO bytesAsInts() map (c => (codec wrap c).toChar)
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

trait WriteChars extends Chars {

  def writeString(string: String,
                  codec: Codec = getCodec(),
                  openOptions: Iterable[OpenOptions] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
  }
  def writeStrings(strings: Traversable[String],
                   codec: Codec = getCodec(),
                   openOptions: Iterable[OpenOptions] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
  }
  def writeLines(strings: Traversable[String],
                 separator: String = compat.Platform.EOL,
                 codec: Codec = getCodec(),
                 openOptions: Iterable[OpenOptions] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
  }

}
