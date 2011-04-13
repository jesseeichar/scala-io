/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection.Traversable
import Line._
import java.net.{URL, URLConnection}
import java.io.{Reader, File}

/**
 * An trait for objects that viewed as a sequence of characters. For example java.io.Reader
 * a ReadChars object (or be converted to a ReadChars object).
 *
 * Note: All collections returned are non-strict collections and each
 * invocation of a method will typically open a new stream or channel.
 * That behaviour can be overrided by the implementation but
 * it is the default behaviour.
 *
 * The Default implementation is based on providing an implementation for
 * chars method and all other methods are implemented using
 * that method.
 *
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see [[scalax.io.Output]]
 * @see [[scalax.io.Input]]
 * @see [[scalax.io.WriteChars]]
 */
trait ReadChars {
  /**
   * The characters in the object.
   *
   * @return
   *          an traversable of all the characters
   */
  def chars: LongTraversable[Char]
  /**
   * Obtain an non-strict traversable for iterating through the lines in the object
   *
   * @param terminator
   *          The strategy for determining the end of line
   *          Default is to auto-detect the EOL
   * @param includeTerminator
   *          if true then the line will end with the line terminator
   *          Default is false
   *
   * @return
   *          a non-strict traversable for iterating through all the lines
   */
  def lines(terminator: Terminators.Terminator = new Terminators.Auto(),
            includeTerminator: Boolean = false): ResourceView[String] = {
             new LineTraversable(chars.iterator, terminator, includeTerminator).view
        }
  /**
   * Loads all the characters into memory. There is no protection against
   * loading very large files/amounts of data.
   */
  def slurpString = chars.mkString

}


object ReadChars {
  class AsBinaryReadChars(op: Codec => ReadChars) {
    /** An object to an ReadChars object */
    def asBinaryReadChars(implicit codec:Codec = Codec.default): ReadChars = op(codec)
  }

  /**
   * Wrap an arbitrary object as and AsReadChars object allowing the object to be converted to an ReadChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsReadCharsConverterFromBinary]]
   */
  implicit def asReadCharsConverter[B](src:B)(implicit converter:AsBinaryReadCharsConverter[B]) =
    new AsBinaryReadChars(codec => converter.toReadChars(src,codec))

      
  /**
   * Used by the [[scalax.io.ReadChars]] object for converting an arbitrary object to an ReadChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsBinaryReadCharsConverter[-A] {
    def toReadChars(t:A,codec:Codec) : ReadChars
  }
  
  /**
   * contains several implementations of [[scalax.io.AsReadCharsConverterFromBinary]].  They will be implicitly resolved allowing
   * a user of the library to simple call A.asBinaryReadChars and the converter will be found without the user needing to look up these classes
   */
  object AsBinaryReadCharsConverter {
  
    /**
     * Converts a File to an ReadChars object
     */
    implicit object FileConverter extends AsBinaryReadCharsConverter[File]{
      def toReadChars(file: File, codec:Codec) = Resource.fromFile(file).reader(codec)
    }
    /**
     * Converts a URL to an ReadChars object
     */
    implicit object URLConverter extends AsBinaryReadCharsConverter[URL]{
      def toReadChars(url: URL, codec:Codec) = Resource.fromURL(url).reader(codec)
    }
  }

    class AsReadChars(op: => ReadChars) {
    /** An object to an ReadChars object */
    def asReadChars: ReadChars = op
  }

  /**
   * Wrap an arbitrary object as and AsReadChars object allowing the object to be converted to an ReadChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsReadCharsConverter]]
   */
  implicit def asReadCharsConverter[B](src:B)(implicit converter:AsReadCharsConverter[B]) =
    new AsReadChars(converter.toReadChars(src))


  /**
   * Used by the [[scalax.io.ReadChars]] object for converting an arbitrary object to an ReadChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsReadCharsConverter[-A] {
    def toReadChars(t:A) : ReadChars
  }

  /**
   * contains several implementations of [[scalax.io.AsReadCharsConverter]].  They will be implicitly resolved allowing
   * a user of the library to simple call A.asReadChars and the converter will be found without the user needing to look up these classes
   */
  object AsReadCharsConverter{

    /**
     * Converts a File to an ReadChars object
     */
    implicit object ReaderConverter extends AsReadCharsConverter[Reader]{
      def toReadChars(reader: Reader) = Resource.fromReader(reader)
    }
    /**
     * Converts a String to a ReadChars object
     */
    implicit object TraversableStringConverter extends AsReadCharsConverter[String]{
      def toReadChars(string: String): ReadChars = TraversableCharConverter.toReadChars(string)
    }
    /**
     * Converts a Traversable[Char] to a ReadChars object
     */
    implicit object TraversableCharConverter extends AsReadCharsConverter[Traversable[Char]]{
      def toReadChars(t: Traversable[Char]): ReadChars = new ReadChars {
        def chars: LongTraversable[Char] = new LongTraversable[Char] {

          protected[io] def iterator: CloseableIterator[Char] = CloseableIterator(t.toIterator)
        }.view
      }
    }
  }
}
