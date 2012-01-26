package scalax.io.processing

import scalax.io.{CloseableIterator, LineTraversable, Line}
import scalax.io.SpecializedBufferedIterator


/**
 * Strategy for [[scalax.io.processing.ProcessorAPI]].line to parse a line.
 *
 * Because line uses case-classes for line parsing, it makes the call to line
 * type safe for the value of A
 *
 * User: jeichar
 * Date: 1/23/12
 * Time: 11:25 AM
 */
private[processing] trait LineParser[A] {
  def nextLine(includeTerm: Boolean, term: Line.Terminators.Terminator, iter: SpecializedBufferedIterator[A]): Seq[A]
}

private[processing] object LineParser {
  implicit object charApiToLineParser extends LineParser[Char] {
    def nextLine(includeTerm: Boolean, term: Line.Terminators.Terminator, iter: SpecializedBufferedIterator[Char]):Seq[Char] = {
      val wrapped = new CloseableIterator[Char]() {
        private[this] val proxy = iter
        @inline final def next = proxy.next
        @inline final def hasNext = proxy.hasNext
        final def doClose = ()
      }
      new LineTraversable(wrapped, term, includeTerm).headOption.getOrElse("").toSeq
    }
  }
} 