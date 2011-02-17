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
import java.io.{OutputStream, File, Writer}

/**
 * A trait for objects that can have expect to have characters written to them. For example a
 * FileWriter can be a WriteChars object (or be converted to one).
 * <p>
 * Note: Each invocation of a method will typically open a new stream or
 * channel.  That behaviour can be overridden by the implementation but
 * it is the default behaviour.
 * </p>
 *
 * @author Jesse Eichar
 * @since 1.0
 *
 * @see [[scalax.io.Output]]
 * @see [[scalax.io.ReadChars]]
 * @see [[scalax.io.Input]]
 */
trait WriteChars {


  protected def writer : WriteCharsResource[Writer]

  /**
   * Write several characters to the underlying object
   */
  def write(characters : TraversableOnce[Char]) : Unit = {
    for (out <- writer) {
      characters foreach out.append
    }
  }

  /**
   * Writes a string. The open options that can be used are dependent
   * on the implementation and implementors should clearly document
   * which option are permitted.
   *
   * @param string
   *          the data to write
   */
  def writeString(string : String) : Unit = {
    for (out <- writer) {
      out write string
    }
  }

  /**
   * Write several strings. The open options that can be used are dependent
   * on the implementation and implementors should clearly document
   * which option are permitted.
   *
   * @param strings
   *          The data to write
   * @param separator
   *          A string to add between each string.
   *          It is not added to the before the first string
   *          or after the last.
   */
  def writeStrings(strings: Traversable[String], separator: String = ""): Unit = {
    for (out <- writer) {
      (strings foldLeft true) {
        case (true, s) =>
          out write s
          false
        case (false, s) =>
          out write separator
          out write s
          false
      }
    }
  }
}


object WriteChars {
  class AsBinaryWriteChars(op: (Codec) => WriteChars) {
    /** An object to an WriteChars object */
    def asBinaryWriteChars(implicit codec:Codec): WriteChars = op(codec)
  }

  /**
   * Wrap an arbitrary object as and AsWriteChars object allowing the object to be converted to an WriteChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsWriteCharsConverter]]
   */
  implicit def asWriteCharsConverter[B](src:B)(implicit converter:AsBinaryWriteCharsConverter[B]) =
    new AsBinaryWriteChars(codec => converter.toWriteChars(src,codec))
    
  /**
   * Used by the [[scalax.io.WriteChars]] object for converting an arbitrary object to an WriteChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsBinaryWriteCharsConverter[-A] {
    def toWriteChars(t:A,codec:Codec) : WriteChars
  }
  
  /**
   * contains several implementations of [[scalax.io.AsWriteCharsConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asWriteChars and the converter will be found without the user needing to look up these classes
   */
  object AsBinaryWriteCharsConverter {
  
    /**
     * Converts a File to an WriteChars object
     */
    implicit object FileConverter extends AsBinaryWriteCharsConverter[File]{
      def toWriteChars(file: File,codec:Codec) = Resource.fromFile(file).writer(codec)
    }

    /**
     * Converts a OutputStream to an WriteChars object
     */
    implicit object OutputStreamConverter extends AsBinaryWriteCharsConverter[OutputStream]{
      def toWriteChars(out: OutputStream,codec:Codec) = Resource.fromOutputStream(out).writer(codec)
    }
  }

    class AsWriteChars(op: => WriteChars) {
    /** An object to an WriteChars object */
    def asWriteChars: WriteChars = op
  }

  /**
   * Wrap an arbitrary object as and AsWriteChars object allowing the object to be converted to an WriteChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsWriteCharsConverter]]
   */
  implicit def asWriteCharsConverter[B](src:B)(implicit converter:AsWriteCharsConverter[B]) =
    new AsWriteChars(converter.toWriteChars(src))

  /**
   * Used by the [[scalax.io.WriteChars]] object for converting an arbitrary object to an WriteChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsWriteCharsConverter[-A] {
    def toWriteChars(t:A) : WriteChars
  }

  /**
   * contains several implementations of [[scalax.io.AsWriteCharsConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asWriteChars and the converter will be found without the user needing to look up these classes
   */
  object AsWriteCharsConverter {

    /**
     * Converts a File to an WriteChars object
     */
    implicit object WriterConverter extends AsWriteCharsConverter[Writer]{
      def toWriteChars(writer: Writer) = Resource.fromWriter(writer)
    }
  }
}
