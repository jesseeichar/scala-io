/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import scalax.io.resource.{
  Resource, InputStreamResource, OutputStreamResource
}
import scalax.io.{
  NotDirectoryException,OpenOption
}
import scala.collection.mutable.ArrayBuffer
import java.io.{
  OutputStream, InputStream, ByteArrayOutputStream, ByteArrayInputStream, IOException
}

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
  def name:String
  def lastModified:Long
}
private[ramfs] class FileNode(val name:String) extends Node {
    var data = ArrayBuffer[Byte]()
    var lastModified = System.currentTimeMillis
    def inputResource : InputStreamResource[InputStream] = 
      Resource.fromInputStream(new ByteArrayInputStream(data.toArray))
      
    def outputResource(openOptions: OpenOption*) : OutputStreamResource[OutputStream] = {
      Resource.fromOutputStream(new ByteArrayOutputStream(){
        override def close() {
          super.close()
          data = ArrayBuffer(this.toByteArray:_*)
        }
      })
    }
    
    def channel() = Resource.fromByteChannel(new SeekableFileNodeChannel(this))
  }

private[ramfs] class DirNode(val name:String) extends Node {
  val children = ArrayBuffer[Node]()
  var lastModified = System.currentTimeMillis
  val self = this;
  
  def lookup(path:Seq[String]) : Option[Node]= path match {
      case Seq(s) if s == name => 
        Some(self)
      case Seq(s, rest @ _*) => 
        children.find {_.name == s} match {
          case Some(x:DirNode) => x.lookup(rest) 
          case _ => None
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
        case None =>
          val newNode = fac create s
          children += newNode
          newNode
      }
    case Seq(s, rest @ _*) => 
      children.find {_.name == path.head} match {
        case Some(f:FileNode) => 
           throw new NotDirectoryException()
        case Some (d:DirNode) => 
          d.create(rest, fac)
        case None =>
          val newNode = new DirNode(s)
          children += newNode
          newNode.create(rest,fac)
      }
  }
  
}