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
