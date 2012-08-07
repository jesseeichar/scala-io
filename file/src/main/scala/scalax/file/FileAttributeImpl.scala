package scalax.file

case class FileAttributeImpl[T](name:String, value:T) extends FileAttribute[T]
