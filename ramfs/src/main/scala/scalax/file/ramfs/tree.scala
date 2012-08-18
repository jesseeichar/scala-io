/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package ramfs

import java.nio.file.attribute.PosixFilePermission
import PosixFilePermission._
import RamAttributes.RamAttribute
import scala.collection.mutable.ArrayBuffer
import java.io.{
  OutputStream, InputStream, ByteArrayOutputStream, ByteArrayInputStream, IOException
}
import scalax.io._
import scalax.io.managed._
import StandardOpenOption._

private[ramfs] trait NodeFac {
  def create(name:String):Node
  def accepts(node:Node):Boolean
}

private[ramfs] object FileNode extends NodeFac {
  def create(name:String) = new FileNode(name)
  def accepts(node:Node) = node.isInstanceOf[FileNode]
}


private[ramfs] object DirNode extends NodeFac {
  def create(name:String) = new DirNode(name)
  def accepts(node:Node) = node.isInstanceOf[DirNode]
}

private[ramfs] trait Node {
  val attributes = collection.mutable.HashMap[RamAttribute,Object]()
  attributes ++= initAccess
  
  var name:String
  def canRead = attributes(RamAttributes.permissions).asInstanceOf[Set[PosixFilePermission]].contains(OWNER_READ)
  def canWrite = attributes(RamAttributes.permissions).asInstanceOf[Set[PosixFilePermission]].contains(OWNER_WRITE)
  def canExecute = attributes(RamAttributes.permissions).asInstanceOf[Set[PosixFilePermission]].contains(OWNER_EXECUTE)

  def lastModified = attributes(RamAttributes.lastModified).asInstanceOf[java.lang.Long]
  def lastModified_=(newVal: Long) = attributes(RamAttributes.lastModified) = newVal:java.lang.Long
  def lastAccessTime = attributes(RamAttributes.lastAccessTime).asInstanceOf[java.lang.Long]  
  def lastAccessTime_=(newVal: Long) = attributes(RamAttributes.lastAccessTime) = newVal:java.lang.Long
  def creationTime = attributes(RamAttributes.creationTime).asInstanceOf[java.lang.Long]  
  def creationTime_=(newVal: Long) = attributes(RamAttributes.creationTime) = newVal:java.lang.Long

  protected def initAccess = Map[RamAttribute, Object](
    RamAttributes.lastModified -> (System.currentTimeMillis:java.lang.Long),
    RamAttributes.lastAccessTime -> (System.currentTimeMillis:java.lang.Long),
    RamAttributes.creationTime -> (System.currentTimeMillis:java.lang.Long),
    RamAttributes.hidden -> (false:java.lang.Boolean),

    RamAttributes.permissions -> Set(OWNER_READ, OWNER_WRITE, GROUP_READ, OTHERS_READ)
  )
  override def toString = getClass.getSimpleName+": "+name
}

private[ramfs] class FileNode(var name:String) extends Node {
    var data = ArrayBuffer[Byte]()
    def channel(openOptions:OpenOption*) = new ArrayBufferSeekableChannel(data, openOptions:_*)(_ => data.clear, _ => lastAccessTime = System.currentTimeMillis)
    def inputStream : InputStream = {
      new ByteArrayInputStream(data.toArray){
        override def close = {
  		  lastAccessTime = System.currentTimeMillis
  		  super.close()
        }
      }
    }
  }

private[ramfs] class DirNode(var name:String) extends Node {
  override protected def initAccess = {
    super.initAccess ++ Map (
      RamAttributes.permissions -> Set(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE, GROUP_READ, GROUP_EXECUTE, OTHERS_READ, OTHERS_EXECUTE)
    )
  }

  val children = ArrayBuffer[Node]()
  val self = this;

  def lookup(path:Seq[String]) : Option[Node]= {
//    println("lookup",name,children,path)
    path match {
      case Seq(s) if s == name =>
        Some(self)
      case Seq(s) =>
        children.find {_.name == s}
      case Seq(s, rest @ _*) if s == name =>
        self.lookup(rest)
      case Seq(s, rest @ _*) =>
        children.find {_.name == s} match {
          case Some(x:DirNode) => x.lookup(rest)
          case _ => None
        }
      }
  }

  def create(path:Seq[String], fac:NodeFac) : Node = path match {
    case Seq(s) if s == name =>
      // TODO Consider a specific type of exception
      if(fac accepts self) throw new IOException(name+" is a DirNode not a file")
      self
    case Seq(s) =>
      children.find {_.name == path.head} match {
        case Some(x) if fac accepts x =>
          // good
          self
        case Some(x) =>
          // TODO specific exception type ??
          throw new IOException(x+" is not a "+fac)
        case None if !canWrite =>
          throw new IOException("Permission denied to create file in "+name)
        case None =>
          val newNode = fac create s
          children += newNode
          newNode
      }
    case Seq(s, rest @ _*) =>
      children.find {_.name == path.head} match {
        case Some(f:FileNode) =>
           throw new NotDirectoryException(f.name)
        case Some(d) if !d.canWrite =>
          throw new IOException("Permission denied to create file in "+name)
        case Some (d:DirNode) =>
          d.create(rest, fac)
        case None if !canWrite =>
          throw new IOException("Permission denied to create file in "+name)
        case None =>
          val newNode = new DirNode(s)
          children += newNode
          newNode.create(rest,fac)
      }
    case _ =>
      throw new Error("uh oh what's going on?")
  }

  def mkString() : String = {
    name + children.map{
      case f:FileNode => name
      case d:DirNode => d.mkString()
    }.mkString("{",",","}")
  }

}
