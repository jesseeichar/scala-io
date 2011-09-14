/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import resource._
import scala.collection.Traversable
import scalax.io.CloseAction.Noop
import java.io.{Writer, FilterOutputStream, File, OutputStream}

/**
 * A trait for objects that can have data written to them. For example an
 * OutputStream and File can be an Output object (or be converted to one).
 *
 * Note: Each invocation of a method will typically open a new stream or
 * channel. That behaviour can be overridden by the implementation but
 * it is the default behaviour.
 *
 * The consequence of a new stream being opened each time a write is performed
 * is different for each implementation.  In the case of a [[scalax.io.Resource]].fromFile
 * reach write will write to the beginning of the file rather than appending to the file.
 *
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see [[scalax.io.ReadChars]]
 * @see [[scalax.io.Input]]
 * @see [[scalax.io.WriteChars]]
 */
trait Output {

  protected def underlyingOutput : OutputResource[OutputStream]
  /**
   * Execute the function 'f' passing an Output instance that performs all operations
   * on a single opened connection to the underlying resource. Typically each call to
   * one of the Output's methods results in a new connection.  For example if the underlying
   * OutputStream truncates the file each time the connection is made then calling write
   * two times will result in the contents of the second write overwriting the second write.
   *
   * Even if the underlying resource is an appending, using open will be more efficient since
   * the connection only needs to be made a single time.
   *
   * @param f the function to execute on the new Output instance (which uses a single connection)
   * @return the result of the function
   */
  def openOutput[U](f:Output=> U):U = {
    underlyingOutput.acquireAndGet {out =>
      val nonClosingOutput:Output = new OutputStreamResource[OutputStream](null,Noop) {
        val instance = new OpenedResource[OutputStream]{
          def close(): List[Throwable] = Nil
          val get = new FilterOutputStream(out){
            override def close() {}
          }
        }
        override def open():OpenedResource[OutputStream] = instance
      }
      f(nonClosingOutput)
    }
  }

  /**
   * Write data to the underlying object.  Each time write is called the resource is reopened, in the case of a
   * file this means that the file will be opened and truncated.  The
   *
   * In the case of writing ints and bytes it is often
   * recommended to write arrays of data since normally the underlying object can write arrays
   * of bytes or integers most efficiently.
   *
   * Since Characters require a codec to write to an OutputStream characters cannot be written with this method
   * unless a OutputWriterFunction.CharFunction object is provided as the writer.
   *
   * @see #writeChars for more on writing characters
   *
   * @param data
   *          The data to write to underlying object.  Any data that has a resolvable [[scalax.io.OutputConverter]] can
   *          be written.  See the [[scalax.io.OutputConverter]] object for the defined [[scalax.io.OutputConverter]]
   *          implementations and classes to assist implementing more.
   * @param writer
   *          The strategy used to write the data to the underlying object.  Many standard data-types are implicitly
   *          resolved and do not need to be supplied
   */
  def write[T](data:T)(implicit writer:OutputConverter[T]):Unit = underlyingOutput.foreach {writer(_,data)}

  /**
   * Since the [[scalax.io.OutputConverter]] object defined for writing Ints encodes Ints using 4 bytes this method
   * is provided to simply write an array of Ints as if they are Bytes.  In other words just taking the first
   * byte.  This is pretty common in Java.io style IO.  IE
   *
   * {{{ outputStream.write(1) }}}
   *
   * 1 is written as a single byte.
   */
  def writeIntsAsBytes(data: Int*) = write(data)(OutputConverter.TraversableIntAsByteConverter)
  /**
  * Writes a string.
  *
  * @param string
  *          the data to write
  * @param codec
  *          the codec of the string to be written. The string will
  *          be converted to the encoding of {@link sourceCodec}
  *          Default is sourceCodec
  */
  def write(string: String)(implicit codec: Codec = Codec.default): Unit = {
      underlyingOutput.writer write string
  }

  /*
   * Writes Characters to the underlying object.
   *
   * @param characters the characters to write
   * @param codec the codec to use for encoding the characters
   */
  def writeChars(characters: TraversableOnce[Char])(implicit codec: Codec = Codec.default) : Unit = {
    write(characters)(OutputConverter.charsToOutputFunction)
  }

  /**
  * Write several strings.
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
  */
  def writeStrings(strings: Traversable[String], separator:String = "")(implicit codec: Codec = Codec.default): Unit = {
      underlyingOutput.writer.writeStrings(strings,separator)
  }
  
  /**
   * Copy data from an input to this output.  Input will call this method from copyData if
   * it does not know how to efficiently copy the data.  This method thus will copy as effiently as
   * possible and if all else fails simply write the bytes
   */
  protected[io] def copyFrom(source:Input):Unit = write (source.bytes)
}

