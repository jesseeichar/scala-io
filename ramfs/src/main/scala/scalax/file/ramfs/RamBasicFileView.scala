package scalax.file.ramfs

import java.nio.file.attribute.BasicFileAttributeView
import java.nio.file.attribute.FileTime
import java.nio.file.NoSuchFileException

class RamBasicFileView(path:RamPath) extends BasicFileAttributeView {
    override val name = getClass.getSimpleName();
    override def readAttributes() = new BasicRamFileAttributes(path)
    override def setTimes(lastModifiedTime: FileTime,
                  lastAccessTime: FileTime,
                  createTime: FileTime) = {
      val node = path.getFileSystem.lookup(path) getOrElse {throw new NoSuchFileException("The RamFS file: "+path+" does not exist")}
      
      node.creationTime = createTime.toMillis()
      node.lastAccessTime = lastAccessTime.toMillis()
      node.lastModified = lastModifiedTime.toMillis()
    }
}