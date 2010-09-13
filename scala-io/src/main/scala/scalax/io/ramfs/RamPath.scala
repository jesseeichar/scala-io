/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import resource.ManagedResource
import scalax.io.{
  FileSystem, Path, FileOps,Codec,PathMatcher,DirectoryStream, LinkOption
}
import scalax.io.attributes.FileAttribute

import Path.AccessModes._
import Path.fail

import java.net.{
  URL,URI
}

class RamPath(relativeTo:String, val path:String, override val fileSystem:RamFileSystem) extends Path(fileSystem) {  
  def node = fileSystem.lookup(this)
  lazy val toAbsolute: Path = fileSystem("",relativeTo + separator + path)
  lazy val toURI: URI = fileSystem.uri(this)
  def \(child: String): RamPath = fileSystem(relativeTo,path + separator + child)
  lazy val name: String = segments.lastOption.getOrElse("/")
  override lazy val normalize = super.normalize.asInstanceOf[RamPath]
  lazy val parent: Option[RamPath] = {
    if(toAbsolute.path == fileSystem.root.path) {
      None
    } else {
      val parentPath = fileSystem(toAbsolute.segments.dropRight(1).mkString(separator,separator,""))
      Some(parentPath)
    }
  }
  def checkAccess(modes: AccessMode*): Boolean = node match {
    case None => 
      false
    case Some(node) =>  
      modes forall {
        case Execute  => node.canExecute 
        case Read     => node.canRead
        case Write    => node.canWrite
      }      
  }
  def exists = node.isDefined
  def isFile = node.map(FileNode.accepts).getOrElse(false)
  def isDirectory = node.map(DirNode.accepts).getOrElse(false)
  def isAbsolute = relativeTo == ""
  def isHidden = false //TODO
  def lastModified = node map {_.lastModified} getOrElse 0
  def lastModified_=(time: Long) = {
    node foreach {_.lastModified = time}
    time
  }
  def size = node collect {case f:FileNode => f.data.size.toLong} getOrElse -1
  def access_=(accessModes:Iterable[AccessMode]) = node match {
    case None => fail("Path %s does not exist".format(path))
    case Some(node) =>  
        if (notExists) fail("Path %s does not exist".format(path))

        node.canRead = accessModes exists {_==Read}
        node.canWrite = accessModes exists {_==Write}
        node.canExecute = accessModes exists {_==Execute}
  }
  
  def doCreateFile ():Boolean = fileSystem.create(this,FileNode, false)
  def doCreateDirectory ():Boolean = fileSystem.create(this, DirNode, false)
  def doCreateParents ():Unit = this.parent.foreach (fileSystem.create(_, DirNode, true))
  def delete(force:Boolean): Path = {
    if(node.collect{case d:DirNode => d.children.isEmpty}.forall{p=>p}) {
      if(exists && !fileSystem.delete(this, force)) {
        fail("Could not delete "+path)
      }
    } else {
      fail("Directory is not empty, cannot delete")
    }
    this
  }
  protected def copyFile(dest: Path): Path = {
    dest match {
      case dest:RamPath => node.foreach{case node:FileNode => fileSystem.copyFile(this, node, dest)}
      case dest =>
        dest.ops.writeInts(ops.bytesAsInts)
    }
    dest
  }
  protected def moveFile(target: Path, atomicMove:Boolean): Unit = fileSystem.move(this, target.asInstanceOf[RamPath])
  protected def moveDirectory(target: Path, atomicMove:Boolean): Unit = fileSystem.move(this, target.asInstanceOf[RamPath])
  
  override def toString() = "RamPath(%s)".format(path)
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }  
  override def hashCode() = path.hashCode()

  def descendants(filter:Path => Boolean, depth:Int, options:Traversable[LinkOption]):DirectoryStream[Path] = new RamDirectoryStream(this,filter,depth)

  def ops:FileOps = new RamFileOps(this)
   
}