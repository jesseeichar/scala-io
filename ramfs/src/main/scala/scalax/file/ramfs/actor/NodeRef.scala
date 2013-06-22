package scalax.file.ramfs.actor

import scalax.file.BasicFileAttributes

class NodeRef (private[actor] val ref: Node, private[actor] val actor: RamFsActor) extends BasicFileAttributes {
  override def lastModifiedTime: FileTime = actor !? RamFsMsg.LastModified
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

}