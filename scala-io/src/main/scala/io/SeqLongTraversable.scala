/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection._
import scala.collection.generic._

class SeqLongTraversable[A](parts:Seq[Traversable[A]]) extends LongTraversable[A] {
  def foreach[U](f: (A) => U): Unit = {
    for (t <- parts; a <- t) f(a)
  }
  
  /*
   * TODO improve performance of drop slice etc...
   *      improvements can be made by implementing these in terms of the contained traversable
   *      right now these will cause all elements to be iterated through
   */
}