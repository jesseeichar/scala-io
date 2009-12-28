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

class ExtendedTraversable[A](wrapped:Iterable[A]) extends Traversable[A] with GenericTraversableTemplate[A,ExtendedTraversable] {
  override def companion = ExtendedTraversable
  def foreach[U](f:A=>U):Unit = wrapped.foreach(f)
}

object ExtendedTraversable extends TraversableFactory[ExtendedTraversable] {  
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ExtendedTraversable[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A] = new scala.collection.mutable.Builder[A,ExtendedTraversable[A]] {
    val buf = new scala.collection.mutable.ListBuffer[A]()
    def +=(a:A) = {buf += a;this}
    def clear = buf.clear
    def result = new ExtendedTraversable(buf.result)
  }
}
