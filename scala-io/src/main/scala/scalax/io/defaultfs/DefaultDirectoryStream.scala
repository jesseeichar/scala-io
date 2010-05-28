/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
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
import java.io.{File => JFile}
private[defaultfs] class DefaultDirectoryStream(parent : DefaultPath, 
                             pathFilter : Path => Boolean,
                             depth:Int) extends DirectoryStream[DefaultPath] {
                               
  assert(parent.isDirectory, "parent of a directory stream must be a Directory")
  lazy val fs = parent.fileSystem

  override def iterator: Iterator[DefaultPath] = new Iterator[DefaultPath] {
    var toVisit = parent.jfile.listFiles.toList
    var nextElem : Option[DefaultPath] = None
    
    def hasNext() = if(nextElem.nonEmpty) true
                    else {
                      nextElem = loadNext()
                      nextElem.nonEmpty
                    }
    
    def loadNext() : Option[DefaultPath] = {
      toVisit match {
        case Nil => None
        case d :: _ if d.isDirectory =>
          val path = fs(d)
          if(depth < 0 || path.relativize(parent).segments.size < depth)
            toVisit = d.listFiles.toList ::: toVisit.tail
          else
            toVisit = toVisit.tail
          Some(path).filter(pathFilter).orElse{loadNext}
        case f :: _ => 
          toVisit = toVisit.tail
          Some(fs(f)).filter(pathFilter).orElse{loadNext}
      }
    }
    
    def next() = {
      val t = nextElem
      nextElem = None
      
      t match {
        case None => throw new NoSuchElementException
        case Some(p) => p
      }
    }
  }
}