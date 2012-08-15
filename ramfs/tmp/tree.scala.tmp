/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package ramfs

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
  val attributes = collection.mutable.HashMap[String,Any]()
  var name:String
  var lastModified:Long
  var (canRead, canWrite, canExecute) = initAccess;
  protected def initAccess = (true, true, false)
  override def toString = getClass.getSimpleName+": "+name
}

private[ramfs] class FileNode(var name:String) extends Node {
    var data = ArrayBuffer[Byte]()
    var lastModified = System.currentTimeMillis
    def inputStream : InputStream = new ByteArrayInputStream(data.toArray)

    def outputResource(owner:RamPath, openOptions: OpenOption*) : OutputStream = {
      require (owner.node exists {_ == this})
      if (!canWrite) {
        throw new IOException("Not permitted to write to this file")
      }

      def newResource = {
        val out = new ByteArrayOutputStream(){
          override def close() {
            super.close()
            if(openOptions contains DeleteOnClose) {
              owner.delete(force=true);
            } else {
              data = ArrayBuffer(this.toByteArray:_*)
            }
          }
        }

        if(openOptions contains Append) out.write(data.toArray,0,data.size);

        out
      }

      if(openOptions contains Truncate) data.clear()

      newResource
    }

    def channel(owner:RamPath, openOptions: OpenOption*) = {
      def newchannel = new ArrayBufferSeekableChannel(this.data,openOptions:_*)(_ => owner.delete(force=true), _ => ())
      newchannel
    }
  }

private[ramfs] class DirNode(var name:String) extends Node {
  override protected def initAccess = (true, true, true)

  val children = ArrayBuffer[Node]()
  var lastModified = System.currentTimeMillis
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
