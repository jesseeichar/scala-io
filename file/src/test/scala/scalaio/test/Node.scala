/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import collection.mutable.ListBuffer

object Node {
  val Sep = "/"
}
case class Node(path : String, parent : Option[Node], children : ListBuffer[Node] = ListBuffer[Node]()) extends Iterable[Node]{
  self =>
  parent.foreach {_.children += self}

  def iterator = children.iterator
  def all = (List[Node]() /: this) {case (acc,next) => acc ++ next}
  def name = path.split(Node.Sep).last

  override def toString = path
}
