/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection.mutable.Buffer
import Line.Terminators._
import annotation.tailrec

/**
 * Creates a Traversable[String] from a Traversable[Char] where each String is a line as indicated by the
 * [[scalax.io.Line.Terminators.Terminator]].
 *
 *
 * @see [[scalax.io.Input]]
 * @see [[scalax.io.ReadChars]]
 */
class LineTraversable(source: => CloseableIterator[Char], terminator: Terminator, includeTerminator: Boolean) extends LongTraversable[String] {

  protected[io] def iterator = new CloseableIterator[String] {
    val sourceIterator = source
    val buffer = Buffer[Char]()
    def next() = {
      var line:String = ""

      while(line == "" && sourceIterator.hasNext) {
        val nextChar = sourceIterator.next()
        terminator.split(buffer :+ nextChar) match {
          case split @ LineSplit(_,_,nextLine) if nextLine.nonEmpty => {
            line = split toString includeTerminator
            buffer.clear
            buffer ++= nextLine
          }
          case _ =>
            buffer += nextChar
        }
      }
      if(line == "") {
        line = terminator.split(buffer).toString(includeTerminator)
        buffer.clear()
      }
      line
    }
    def hasNext: Boolean = sourceIterator.hasNext || buffer.nonEmpty

    def doClose() = sourceIterator.close()
  }

}
