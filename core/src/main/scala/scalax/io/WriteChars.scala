/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import resource._
import scala.collection.Traversable
import scalax.io.CloseAction.Noop
import java.io._
import collection.immutable.StringLike
import scalax.io.managed.WriterResource

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


  protected def writer : Resource[Writer]
  /**
   * Execute the function 'f' passing an WriteChars instance that performs all operations
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
  def writeCharsProcessor = new processing.WriteCharsProcessor(writer)

  /**
   * Write several characters to the underlying object
   */
  def write(characters : TraversableOnce[Char]) : Unit = {
    for (out <- writer) {
      characters match {
        case string:StringLike[_] => out.write(string.toString)
        case _ => characters foreach out.append
      }
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
