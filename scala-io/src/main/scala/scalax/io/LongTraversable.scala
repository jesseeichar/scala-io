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

import scala.collection.mutable.{ Builder, ListBuffer }

/**
 * A traversable for use on very large datasets which cannot be indexed with Ints but instead
 * require Longs for indexing.  This trait adds methods for accessing the extra portions
 * of the dataset.
 */
trait LongTraversable[+A] extends Traversable[A] 
                            with GenericTraversableTemplate[A, LongTraversable]
                            with LongTraversableLike[A, LongTraversable[A]] {

  override def companion : GenericCompanion[LongTraversable] = LongTraversable
}


object LongTraversable extends TraversableFactory[LongTraversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LongTraversable[A]] = new GenericCanBuildFrom[A]
  
  // TODO consider a correct implementation
  def newBuilder[A]: Builder[A, LongTraversable[A]] = new ListBuffer[A] mapResult (x => new LongTraversableImpl[A](x))
}

private class LongTraversableImpl[A](contained:Traversable[A]) extends LongTraversable[A] {
  def foreach[U](f: A => U): Unit = contained foreach f
}