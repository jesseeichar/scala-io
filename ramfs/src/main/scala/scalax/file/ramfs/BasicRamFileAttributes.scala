package scalax.file.ramfs

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.NoSuchFileException
import java.nio.file.attribute.FileTime

class BasicRamFileAttributes(path: RamPath) extends BasicFileAttributes {
  
    private def lookup = path.getFileSystem.lookup(path) getOrElse {throw new NoSuchFileException("The RamFS file: "+path+" does not exist")}
	override def lastModifiedTime: FileTime = FileTime.fromMillis(lookup.lastModified)
    override def lastAccessTime: FileTime = FileTime.fromMillis(lookup.lastAccessTime)
    override def creationTime: FileTime = FileTime.fromMillis(lookup.creationTime)

    override def isRegularFile() = lookup.isInstanceOf[FileNode]
    override def isDirectory() = lookup.isInstanceOf[DirNode]
    override def isSymbolicLink() = false
    override def isOther() = false

    override def size(): Long = lookup match {
      case fn: FileNode => fn.data.size.toLong
      case _ => throw new NoSuchFileException("The RamFS file: "+path+" is not a file")
    }

    override val fileKey: Object = null
}