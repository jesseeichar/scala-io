/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.annotation.tailrec

/**
 * Provides some convenience methods for certain operations on TraversableOnce
 */
private[io] object TraversableOnceOps {
  def splitAt[T](data:TraversableOnce[T], index:Int):(TraversableOnce[T],TraversableOnce[T]) = {
    data match {
      case t:Traversable[_] => t.asInstanceOf[Traversable[T]].splitAt(index)
      case _ =>
        val iter = data.toIterator
//        @tailrec
//        def take(container:Vector[T], remaining:Int) = if (remaining > 0){
//          
//        } else {
//          
//        }
//        iter.c
        (iter.take(index).toList,iter)
    }
  }
  def drop[T](data:TraversableOnce[T], length:Int) = {
    data match {
      case t:Traversable[_] => t.asInstanceOf[Traversable[T]] drop length
      case _ => data.toIterator drop length
    }
  }
}
