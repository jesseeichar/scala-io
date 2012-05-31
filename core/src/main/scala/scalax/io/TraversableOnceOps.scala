/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

/**
 * Provides some convenience methods for certain operations on TraversableOnce
 */
private[io] object TraversableOnceOps {
  def splitAt[T](data:TraversableOnce[T], index:Int):(TraversableOnce[T],TraversableOnce[T]) = {
    data match {
      case t:Traversable[_] => t.asInstanceOf[Traversable[T]].splitAt(index)
      case _ =>
        val iter = data.toIterator
        val builder = new VectorBuilder[T]()
        var i = 0
        while(i < index && iter.hasNext) {
          builder += iter.next()
          i += 1
        }
        (builder.result,iter)
    }
  }
  def drop[T](data:TraversableOnce[T], length:Int) = {
    data match {
      case t:Traversable[_] => t.asInstanceOf[Traversable[T]] drop length
      case _ => data.toIterator drop length
    }
  }
}
