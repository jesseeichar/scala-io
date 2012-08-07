package scalax.file
package defaultfs

import collection.JavaConverters._
import scalax.file.FileAttributes
import scala.reflect.ClassTag
import java.nio.file.{Files => JFiles}
import scala.util.control.Exception._

class DefaultFileAttributes(val path: DefaultPath) extends FileAttributes {
  val jpath = path.jfile
  override def all(linkOptions:LinkOption*): Set[FileAttribute[_]] = 
    JFiles.readAttributes(jpath, "*", linkOptions:_*).asScala.map{case (key,value) => FileAttributeImpl(key, value)}.toSet
  override def apply[A](name: String, linkOptions:LinkOption*): Option[A] = 
    Option(JFiles.getAttribute(jpath, name, linkOptions:_*).asInstanceOf[A]) 
  override def get[T <: BasicFileAttributes](c: Class[T], linkOptions:LinkOption*): Option[T] = {
    catching(classOf[UnsupportedOperationException]).opt{JFiles.readAttributes(jpath, c, linkOptions:_*)}
  }
  
  override def update(name: String, newVal: Any, linkOptions:LinkOption*): this.type = {
    JFiles.setAttribute(jpath, name, newVal, linkOptions:_*)
    this
  }

  override def view[T <: FileAttributeView](c: Class[T], linkOptions:LinkOption*): Option[T] = {
    catching(classOf[UnsupportedOperationException]).opt{JFiles.getFileAttributeView(jpath, c, linkOptions:_*)}
  }

  override def supportsView[T <: FileAttributeView](c:Class[T]):Boolean = {
    jpath.getFileSystem.getFileStores.asScala.exists(_.supportsFileAttributeView(c))
  }
  override val supportedViewNames: Set[String] = 
    jpath.getFileSystem.supportedFileAttributeViews.asScala.toSet

}