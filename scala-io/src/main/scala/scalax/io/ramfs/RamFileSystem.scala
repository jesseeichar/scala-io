/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import scala.resource.ManagedResource
import scalax.io.{
  FileSystem, Path, FileOps,Codec,PathMatcher,DirectoryStream
}
import scalax.io.attributes.FileAttribute

import Path.AccessModes._
import java.net.{
  URL,URI
}
import java.util.UUID

class RamFileSystem extends FileSystem {
  private var fsTree = new DirNode(separator)
  
  val id = UUID.randomUUID
  val root = new RamPath("",fsTree.name, this)
  var pwd = root
  
  def separator: String = "/"
  def apply(path: String): RamPath = {
    if(path startsWith separator) apply("",path)
    else apply(pwd.toAbsolute.path, path)
  }
  def apply(relativeTo:String , path: String): RamPath = {
    def process(p:String) = {
      val p = path.replaceAll(separator+"+", separator)
      if(path endsWith separator) p.drop(1)
      else p
    }

    new RamPath(process(relativeTo), process(path), this)
  } 
  def roots:List[RamPath] = List (root)
  def createTempFile(prefix: String = randomPrefix, 
                   suffix: String = null, 
                   dir: String = null,
                   deleteOnExit : Boolean = true
                   /*attributes:List[FileAttributes] TODO */ ) : Path = new RamPath("temp",UUID.randomUUID.toString,this)

  def createTempDirectory(prefix: String = randomPrefix,
                        suffix: String = null, 
                        dir: String = null,
                        deleteOnExit : Boolean = true
                        /*attributes:List[FileAttributes] TODO */) : Path  = new RamPath("temp",UUID.randomUUID.toString,this)
                        
  def matcher(pattern:String, syntax:String = PathMatcher.StandardSyntax.GLOB): PathMatcher = null // TODO

  def uri(path:RamPath = root):URI = new URI("ramfs://"+id+path)
  override def toString = "Ram File System"
  
  protected[ramfs] def lookup(path:RamPath) = {
    val absolutePathMinusRoot = path.toAbsolute.segments.drop(1)
    fsTree.lookup(absolutePathMinusRoot)
  }
    

}
