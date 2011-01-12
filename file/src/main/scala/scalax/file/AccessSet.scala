/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file

import scala.collection.mutable.{Set => MSet}
import Path.AccessModes._

class AccessSet(owner:Path) extends MSet[AccessMode] {

  def -=(elem: AccessMode):AccessSet.this.type = {
    owner.access = owner.access.filterNot{_ == elem}
    this
  }

  def  +=(elem: AccessMode):AccessSet.this.type = {
    owner.access = owner.access + elem
    this
  }

  def contains(elem: AccessMode):Boolean = {
    iterator.contains(elem)
  }

  def iterator = Path.AccessModes.values.iterator filter {
    case Read => owner.canRead
    case Write => owner.canWrite
    case Execute => owner.canExecute
  }

}
