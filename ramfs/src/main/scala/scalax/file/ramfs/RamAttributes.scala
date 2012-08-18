package scalax.file.ramfs
object RamAttributes extends Enumeration {
  type RamAttribute = Value
  
  // basic
  val lastModified, lastAccessTime, creationTime, size, isRegularFile, isDirectory, isSymbolicLink, isOther, fileKey = Value
  // posix
  val group,permissions = Value
  // other
  val hidden = Value
}