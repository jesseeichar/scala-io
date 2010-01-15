/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scala.resource.ManagedResource
import scalax.io.{
  FileSystem, Path, FileOps,Codec,PathMatcher,DirectoryStream
}
import scalax.io.attributes.FileAttribute

import Path.AccessModes._
import java.net.{
  URL,URI
}
class RamFileSystem extends FileSystem {
    def separator: String = "/"
    def apply(path: String): RamPath = new RamPath(path,this)
    def roots:List[RamPath] = null // TODO
    def createTempFile(prefix: String = randomPrefix, 
                     suffix: String = null, 
                     dir: String = null,
                     deleteOnExit : Boolean = true
                     /*attributes:List[FileAttributes] TODO */ ) : Path = new RamPath("temp",this)

    def createTempDirectory(prefix: String = randomPrefix,
                          suffix: String = null, 
                          dir: String = null,
                          deleteOnExit : Boolean = true
                          /*attributes:List[FileAttributes] TODO */) : Path  = new RamPath("temp",this)
                          
    def matcher(pattern:String, syntax:String = PathMatcher.StandardSyntax.GLOB): PathMatcher = null // TODO

    override def toString = "Ram File System"
}
class RamPath(name:String, fileSystem:FileSystem) extends Path(fileSystem) {
    def toAbsolute: Path = null // TODO
    def toURI: URI = null // TODO
    def /(child: String): Path = null // TODO
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
    def length = 0 //TODO
    def access_=(accessModes:Iterable[AccessMode]) = () // TODO
    def access : Set[AccessMode] = null
    def createFile(createParents: Boolean = true, failIfExists: Boolean = true,
                   accessModes:Iterable[AccessMode]=List(READ,WRITE), 
                   attributes:Iterable[FileAttribute[_]]=Nil): Path = null //TODO
    def createDirectory(createParents: Boolean = true, failIfExists: Boolean = true,
                        accessModes:Iterable[AccessMode]=List(READ,WRITE), 
                        attributes:Iterable[FileAttribute[_]]=Nil) = null //TODO
    def delete(): Unit = () //TODO
    def deleteRecursively(continueOnFailure:Boolean=false): (Int,Int) = null //TODO
    def copyTo(target: Path, copyAttributes:Boolean=true, 
               replaceExisting:Boolean=false): Path = null //TODO
    def moveTo(target: Path, replaceExisting:Boolean=false, 
               atomicMove:Boolean=false): Path = null // TODO
    def execute(args:String*)(implicit configuration:ProcessBuilder=>Unit = p =>()):Option[scalax.io.Process] = null // TODO

    override def toString() = "RamPath(%s)".format(path)
    override def equals(other: Any) = other match {
      case x: Path  => path == x.path
      case _        => false
    }  
    override def hashCode() = path.hashCode()

    def directoryStream(filter:Option[PathMatcher] = None):DirectoryStream[Path] = null // TODO

    def tree(filter:(Path,Path)=>Option[PathMatcher], depth:Int):DirectoryStream[Path] = null //TODO

    def fileOps(implicit codec:Codec):FileOps = null //TODO
}