package scalax.file.ramfs

import java.util.{Map => JMap, Set => JSet}
import java.nio._
import channels._
import file._
import attribute._
import java.net._

class RamFsProvider extends file.spi.FileSystemProvider {
  override def getScheme(): String = "ramfs"
  override def newFileSystem(uri: URI, env: JMap[String, _]): FileSystem = {
    if(RamFileSystem.existsFileSystem(uri)) throw new FileSystemAlreadyExistsException("Filesystem with uri: '"+uri+"' already exists")
    RamFileSystem(RamFsId(uri.getUserInfo()))
  }
  override def getFileSystem(uri: URI): FileSystem = RamFileSystem(RamFsId(uri.getUserInfo()))
  override def getPath(uri: URI): Path = RamFileSystem(uri)
  
  
  override def newByteChannel(path: Path, options: JSet[_ <: OpenOption], attrs: FileAttribute[_]*): SeekableByteChannel = null.asInstanceOf[SeekableByteChannel] 
  override def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[_ >: Path]): DirectoryStream[Path] = null.asInstanceOf[DirectoryStream[Path]]
  override def createDirectory(dir: Path, attrs: FileAttribute[_]*): Unit = ()
  override def delete(path: Path): Unit = ()
  override def copy(source: Path, target: Path, options: CopyOption*): Unit = ()
  override def move(source: Path, target: Path, options: CopyOption*): Unit = ()
  override def isSameFile(path: Path, path2: Path): Boolean = false
  override def isHidden(path: Path): Boolean = false
  override def getFileStore(path: Path): FileStore = null.asInstanceOf[FileStore]
  override def checkAccess(path: Path, modes: AccessMode*): Unit = ()
  override def getFileAttributeView[V <: FileAttributeView](path: Path, typeOf: Class[V], options: LinkOption*): V = null.asInstanceOf[V]
  override def readAttributes[A <: BasicFileAttributes](path: Path, typeOf: Class[A], options: LinkOption*): A = null.asInstanceOf[A]
  override def readAttributes(path: Path, attributes: String, options: LinkOption*): JMap[String, Object] = null.asInstanceOf[JMap[String, Object]]
  override def setAttribute(path: Path, attribute: String, value: Any, options: LinkOption*): Unit = ()
}