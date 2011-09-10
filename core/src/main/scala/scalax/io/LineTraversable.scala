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

  protected[io] def iterator = terminator match {
    case Auto => new AutoCharIter(source,includeTerminator)
    case t:SimpleTerminator if t.sep.size == 1 => new SingleCharIter(source, t.sep.head, includeTerminator) 
    case t:SimpleTerminator => new MultiCharIter(source, t.sep, includeTerminator) 
  }
    

}

private[io] class SingleCharIter(sourceIter: CloseableIterator[Char], term:Char, includeTerm:Boolean) extends CloseableIterator[String] {
  private[this] val sb = new StringBuilder

  def getc() = sourceIter.hasNext && {
    val ch = sourceIter.next
    if (ch == term) false
    else {
      sb append ch
      true
    }
  }
  def hasNext = sourceIter.hasNext
  def next = {
    sb.clear
    while (getc()) { }
    if(includeTerm) sb append term
    sb.toString
  }
  
  def doClose = sourceIter.close()
}

private[io] class MultiCharIter(sourceIter: CloseableIterator[Char], term:String, includeTerm:Boolean) extends CloseableIterator[String] {
  private[this] val sb = new StringBuilder

  def getc() = sourceIter.hasNext && {
    sb append sourceIter.next
    
    if (sb.endsWith(term)) false
    else {
      true
    }
  }
  def hasNext = sourceIter.hasNext
  def next = {
    sb.clear
    while (getc()) { }
    if(includeTerm) sb.toString
    else sb.substring(0,sb.length-term.length).toString
  }
  def doClose = sourceIter.close()
}

private[io] class AutoCharIter(sourceIter: CloseableIterator[Char], includeTerm:Boolean) extends CloseableIterator[String] {
  private[this] val sb = new StringBuilder
  
  val iter = sourceIter.buffered
  
  def getc() = {
    if(iter.hasNext) {
        val ch = iter.next
        if (ch == '\n') "\n"
        else if (ch == '\r') {
          if (iter.hasNext && iter.head == '\n') {
            iter.next
            "\r\n"
          } else {
            "\r"        
          }
        }
        else {
          sb append ch
          null
        }
      } else {
        ""
      }
  }
  def hasNext = iter.hasNext
  def next = {
    sb.clear
    var term:String = null
    while (term == null) {
      term = getc()
    }
    if(includeTerm) sb append term
    sb.toString
  }
  def doClose = sourceIter.close()
}