package scalax.file.ramfs

import RamAttributes._
object RamAttributesViews {
  case class RamAttributeView(val name: String, attributes: RamAttribute*)
  val basic = new RamAttributeView("basic",
    lastModified, lastAccessTime, creationTime, size, isRegularFile, isDirectory,
    isSymbolicLink, isOther, fileKey)
  
  val posix = new RamAttributeView("posix", group, permissions)
      
  val views = Seq(basic, posix)
  
  def get(name: String) = views.find(_.name == name)
}