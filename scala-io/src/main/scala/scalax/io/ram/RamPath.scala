/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ram

import scalax.resource.ManagedResource
import scalax.io.{
  FileSystem, Path, FileOps,Codec,PathMatcher,DirectoryStream, LinkOption
}
import scalax.io.attributes.FileAttribute

import Path.AccessModes._
import java.net.{
  URL,URI
}

class RamPath(name:String, fileSystem:FileSystem) extends Path(fileSystem) {
    def toAbsolute: Path = null // TODO
    def toURI: URI = null // TODO
    def \(child: String): Path = null // TODO
    def name: String = null //TODO
    def path: String = null //TODO
    def normalize: Path = null //TODO
    def parent: Option[Path] = null //TODO
    def checkAccess(modes: AccessMode*): Boolean = false //TODO
    def canWrite = false // TODO
    def canRead = false // TODO
    def canExecute = false //TODO
    def exists = false //TODO
    def isFile = false //TODO
    def isDirectory = false //TODO
    def isAbsolute = false //TODO
    def isHidden = false //TODO
    def lastModified = 0 //TODO
    def lastModified_=(time: Long) = 0 //TODO
    def size = 0 //TODO
    def access_=(accessModes:Iterable[AccessMode]) = () // TODO
    def access : Set[AccessMode] = null
    def createFile(createParents: Boolean = true, failIfExists: Boolean = true,
                   accessModes:Iterable[AccessMode]=List(READ,WRITE), 
                   attributes:Iterable[FileAttribute[_]]=Nil): Path = null //TODO
    def createDirectory(createParents: Boolean = true, failIfExists: Boolean = true,
                        accessModes:Iterable[AccessMode]=List(READ,WRITE), 
                        attributes:Iterable[FileAttribute[_]]=Nil) = null //TODO
    def delete(force:Boolean): Path = this //TODO
    def copyTo(target: Path, 
               createParents : Boolean = true,
               copyAttributes:Boolean=true, 
               replaceExisting:Boolean=false): Path = null //TODO
     protected def moveFile(target: Path, atomicMove:Boolean): Unit = null // TODO
     protected def moveDirectory(target: Path, depth:Int, atomicMove:Boolean): Unit = null // TODO
    def execute(args:String*)(implicit configuration:ProcessBuilder=>Unit = p =>()):Option[scalax.io.Process] = null // TODO

    override def toString() = "RamPath(%s)".format(path)
    override def equals(other: Any) = other match {
      case x: Path  => path == x.path
      case _        => false
    }  
    override def hashCode() = path.hashCode()

    def descendants(filter:Path => Boolean, depth:Int, options:Traversable[LinkOption]):DirectoryStream[Path] = null //TODO

    def ops:FileOps = null //TODO
}