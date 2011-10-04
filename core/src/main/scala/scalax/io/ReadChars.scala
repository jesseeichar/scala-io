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
  def lines(terminator: Terminators.Terminator = Terminators.Auto,
            includeTerminator: Boolean = false): LongTraversable[String] = {
             new LineTraversable(chars.iterator, terminator, includeTerminator)
        }
  /**
   * Loads all the characters into memory. There is no protection against
   * loading very large files/amounts of data.
   */
  def slurpString = chars.mkString

}


