/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import scalax.io.{
  AbstractPathPathSet,Path
}

class RamPathSet(
    path:RamPath, 
    pathFilter : Path => Boolean,
    depth:Int) 
  extends AbstractPathPathSet[RamPath](
    path,
    pathFilter,
    depth,
    parent => {
      parent.node.collect {
        case d:DirNode => 
          d.children.map(n => parent \ n.name)
        case _ => 
          throw new AssertionError("This method should only be called on directories")
      }.flatten.toList
    }
  )