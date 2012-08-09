package scalax.file

import collection.JavaConverters._
import scala.reflect.ClassTag
import java.nio.file.{Files => JFiles}
import scala.util.control.Exception._
import java.nio.file.attribute.{FileTime => JFileTime}

class FileAttributes(path: Path) {
  import path.jpath
  def all(implicit linkOptions:Seq[LinkOption] = Seq.empty): Set[FileAttribute[Any]] = {
    val map = JFiles.readAttributes(jpath, "*", linkOptions:_*).asScala
    map.map{case (key,value) => FileAttributeImpl(key, value.asInstanceOf[Any])}.toSet
  }
  def apply(name: String)(implicit linkOptions:Seq[LinkOption] = Seq.empty): Option[Any] = {
    Option(JFiles.getAttribute(jpath, name, linkOptions:_*)).map{
      case time:JFileTime => FileTime(time)
      case v => v 
    }
  }
  def exists[A](att: FileAttribute[A])(implicit linkOptions:Seq[LinkOption] = Seq.empty): Boolean = {
    apply(att.name)(linkOptions) exists {v =>
      v == att.value
      }
  }

  def get[T <: BasicFileAttributes : ClassTag](implicit linkOptions:Seq[LinkOption] = Seq.empty): Option[T] =
    get(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]], linkOptions:_*)
  def get[T <: BasicFileAttributes](c: Class[T], linkOptions:LinkOption*): Option[T] = {
    catching(classOf[UnsupportedOperationException]).opt{JFiles.readAttributes(jpath, c, linkOptions:_*)}
  }

  def update[A](attribute:FileAttribute[A], linkOptions:LinkOption*): this.type = update(attribute.name, attribute.value, linkOptions:_*)
  def update(name: String, newVal: Any, linkOptions:LinkOption*): this.type = {
    val finalVal = newVal match {
      case time: FileTime => time.jfileTime
      case v => v
    }
    JFiles.setAttribute(jpath, name, finalVal, linkOptions:_*)
    this
  }

  def view[T <: FileAttributeView : ClassTag](implicit linkOptions:Seq[LinkOption] = Seq.empty): Option[T] = 
    view(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]], linkOptions:_*)
  def view[T <: FileAttributeView](c: Class[T], linkOptions:LinkOption*): Option[T] = {
    catching(classOf[UnsupportedOperationException]).opt{JFiles.getFileAttributeView(jpath, c, linkOptions:_*)}
  }
  
  def supportsView[T <: FileAttributeView : ClassTag]:Boolean = 
    supportsView(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])
  def supportsView[T <: FileAttributeView](c:Class[T]):Boolean = {
    jpath.getFileSystem.getFileStores.asScala.exists(_.supportsFileAttributeView(c))
  }
  def supportsView(name:String):Boolean = supportedViewNames.exists(_ == name)
  def supportedViewNames: Set[String] = {
    jpath.getFileSystem.supportedFileAttributeViews.asScala.toSet
  }
}