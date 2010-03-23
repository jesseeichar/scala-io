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
 * Provides a default way of resolving which codec to use.  The abstract sourceCodec
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
trait CodecDependent {
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
    def getCodec(givenCodec: Codec = null, allowDefault: Boolean = true) = {
        if (givenCodec != null) givenCodec
        else if (sourceCodec != null) sourceCodec
        else if (allowDefault) Codec.default
        else failNoCodec()
    }

    /**
    * Creates a new BasicFileOperations object with
    * the new codec
    */
    def withCodec(codec:Codec): CodecDependent
}
