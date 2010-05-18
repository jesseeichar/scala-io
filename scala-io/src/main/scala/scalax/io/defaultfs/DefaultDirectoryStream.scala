/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.defaultfs

import scalax.resource.ManagedResource
import scalax.io.{
  FileSystem, Path, FileOps,Codec,PathMatcher,DirectoryStream
}

import scalax.io.attributes.FileAttribute

import Path.AccessModes._
import java.net.{
  URL,URI
}
private[defaultfs] class DefaultDirectoryStream(parent : DefaultPath, 
                             filter : Path => Boolean,
                             depth:Int) extends DirectoryStream[DefaultPath] {
                               
  assert(parent.isDirectory, "parent of a directory stream must be a Directory")
  override def iterator: Iterator[DefaultPath] = new Iterator[DefaultPath] {
    var toVisit = parent.jfile.listFiles.toList
    
    def hasNext = toVisit.nonEmpty
    
    def next = {
      toVisit match {
        case Nil => throw new NoSuchElementException()
        
        // TODO check case where matcher does not match...
        // path passed to matcher must be relative to parent
        case d :: _ if d.isDirectory =>
          val path = parent.fileSystem(d)
          if(depth < path.relativize(parent).segments.size)
            toVisit = d.listFiles.toList ::: toVisit.tail
          else 
            toVisit = toVisit.tail
          path
        case f :: _ => 
          toVisit = toVisit.tail
          parent.fileSystem(f)
      }
    }
  }
}