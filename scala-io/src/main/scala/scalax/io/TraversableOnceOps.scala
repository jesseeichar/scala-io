/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

/**
 * Provides some convenience methods for certain operations on TraversableOnce
 * 
 * User: jeichar
 * Date: Sep 15, 2010
 * Time: 2:24:48 PM
 */
private[io] object TraversableOnceOps {
  def splitAt[T](data:TraversableOnce[T], index:Int):(TraversableOnce[T],TraversableOnce[T]) = {
    data match {
      case t:Traversable[_] => t.asInstanceOf[Traversable[T]].splitAt(index)
      case _ =>
        val iter = data.toIterator
        (iter.take(index).toList,iter)
    }
  }
}