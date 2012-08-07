package scalax.file

import scala.reflect.ClassTag

trait FileAttributes {
  def all: Set[FileAttribute[_]] = all()
  def all(linkOptions:LinkOption*): Set[FileAttribute[_]]
  def apply[A](name: String, linkOptions:LinkOption*): Option[A]
  def exists[A](att: FileAttribute[A], linkOptions:LinkOption*): Boolean = apply(att.name, linkOptions:_*) exists {_ == att.value}

  def get[T <: BasicFileAttributes : ClassTag]: Option[T] = get[T]()
  def get[T <: BasicFileAttributes : ClassTag](linkOptions:LinkOption*): Option[T] =
    get(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]], linkOptions:_*)
  def get[T <: BasicFileAttributes](c: Class[T], linkOptions:LinkOption*): Option[T]

  def update(name: String, newVal: Any, linkOptions:LinkOption*): this.type
  def update[A](attribute:FileAttribute[A], linkOptions:LinkOption*): this.type = update(attribute.name, attribute.value, linkOptions:_*)

  def view[T <: FileAttributeView : ClassTag]: Option[T] = view[T]() 
  def view[T <: FileAttributeView : ClassTag](linkOptions:LinkOption*): Option[T] = 
    view(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]], linkOptions:_*)
  def view[T <: FileAttributeView](c: Class[T], linkOptions:LinkOption*): Option[T] 
  
  def supportsView[T <: FileAttributeView : ClassTag]:Boolean = 
    supportsView(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])
  def supportsView[T <: FileAttributeView](c:Class[T]):Boolean
  def supportsView(name:String):Boolean = supportedViewNames.exists(_ == name)
  def supportedViewNames: Set[String]
}