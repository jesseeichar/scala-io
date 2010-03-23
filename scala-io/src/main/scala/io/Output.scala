/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.io.resource._
import scalax.resource._
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
 * A trait for objects that can have data written to them. For example an
 * OutputStream and File can be an Output object (or be converted to one).
 * Depending on the implementation and the underlying object the
 * {@link OpenOptions} may be restricted to a subset of the
 * {@link OpenOption}.
 * <p>
 * Note: Each invocation of a method will typically open a new stream or
 * channel.  That behaviour can be overrided by the implementation but
 * it is the default behaviour.
 * </p>
 *
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see ReadBytes
 * @see Input
 */
trait Output extends CodecDependent {

    /**
    * Write bytes to the file
    *
    * <strong>Important:</strong> The use of an Array is highly recommended
    * because normally arrays can be more efficiently written using
    * the underlying APIs
    * </p><p>
    * The bytes are either appended to the file or replace the contents of the
    * file depending on the openOptions. By default the contents of the file
    * will be replaced.
    * </p>
    *
    * @param bytes
    *          The bytes to write to the file
    * @param openOptions
    *          The options declaring how the file will be opened
    *          Default is WRITE/CREATE/TRUNCATE
    */
    def writeBytes(bytes: Traversable[Byte],
             openOptions : Seq[OpenOption] = WRITE_TRUNCATE): Unit = {
        for (out <- outputStream(openOptions:_*)) {
            bytes foreach {i => out write i.toInt}
        }
    }

    protected def outputStream(openOptions : OpenOption*) : ManagedResource[OutputStream]

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
                  openOptions: Traversable[OpenOption] = WRITE_TRUNCATE): Unit = {
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
                   openOptions: Traversable[OpenOption] = WRITE_TRUNCATE): Unit = {
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
                 openOptions: Traversable[OpenOption] = WRITE_TRUNCATE): Unit = {
    // TODO
    ()
    }
}
