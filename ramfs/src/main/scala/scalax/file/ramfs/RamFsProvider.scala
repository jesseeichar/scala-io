package scalax.file.ramfs

import java.util.{Map => JMap, Set => JSet}
import java.nio._
import channels._
import file._
import attribute._
import java.net._
import java.io.{IOException, FileNotFoundException}
import collection.JavaConverters._

class RamFsProvider extends file.spi.FileSystemProvider {
  override def getScheme(): String = "ramfs"
  override def newFileSystem(uri: URI, env: JMap[String, _]): FileSystem = {
    if(RamFileSystem.existsFileSystem(uri)) throw new FileSystemAlreadyExistsException("Filesystem with uri: '"+uri+"' already exists")
    RamFileSystem(RamFsId(uri.getUserInfo()))
  }
  override def getFileSystem(uri: URI): FileSystem = RamFileSystem(RamFsId(uri.getUserInfo()))
  override def getPath(uri: URI): Path = RamFileSystem.createPath(uri)
  override def isSameFile(path: Path, path2: Path): Boolean = (path, path2) match {
    case (p1:RamPath, p2:RamPath) =>
      p1.toRealPath().compareTo(p2.toRealPath()) == 0
    case _ => false
  }
  
  override def createDirectory(dir: Path, attrs: FileAttribute[_]*): Unit = {
    val rPath = ramPath(dir)
    rPath.getFileSystem.create(rPath, DirNode, false, toMap(attrs))
  }
  override def delete(path: Path): Unit = {
    val rPath = ramPath(path)
    val node = rPath.getFileSystem.lookup(rPath)
    if (node.forall{n =>
        n.isInstanceOf[FileNode] || n.asInstanceOf[DirNode].children.isEmpty}) {
      if (rPath.exists && !rPath.getFileSystem.delete(rPath, false)) {
        throw new IOException("Could not delete " + path)
      }
    } else {
      throw new IOException("Directory is not empty, cannot delete")
    }
  }
  
  override def copy(source: Path, target: Path, options: CopyOption*): Unit = {
    val Seq(sRPath, tRPath) = ramPath(source, target)
	
	sRPath.getFileSystem.copyFile(sRPath, tRPath)
  }
  override def move(source: Path, target: Path, options: CopyOption*): Unit = {
    val Seq(sRPath, tRPath) = ramPath(source, target)
    sRPath.getFileSystem.move(sRPath, tRPath)
  }
  override def isHidden(path: Path): Boolean = readAttribute(ramPath(path), RamAttributes.hidden).exists(_ == true)
  override def getFileStore(path: Path): FileStore = ramPath(path).getFileSystem.getFileStores.iterator().next()
  override def checkAccess(path: Path, modes: AccessMode*): Unit = {
    val rpath = ramPath(path)
    val treeNode = rpath.getFileSystem.lookup(rpath) getOrElse (throw new FileNotFoundException(path+" Does not exist"))
    val atts = modes map {
      case AccessMode.READ => treeNode.canRead
      case AccessMode.EXECUTE => treeNode.canExecute
      case AccessMode.WRITE => treeNode.canWrite
    }
    
    val legalAccess = atts.forall(b => b)
  }
  override def readAttributes(path: Path, attributes: String, options: LinkOption*): JMap[String, Object] = {
    val (view, atts) = attributes.split(":", 2) match {
      case Array(view, atts) =>
        (RamAttributesViews.get(view) getOrElse (throw new IllegalArgumentException("no view found with name: "+view)), atts)
      case Array(atts) =>
        (RamAttributesViews.basic, atts)
    }
    
    val rpath = ramPath(path)
    val readAttributes = view.attributes.map{att =>
      att.toString -> readAttribute(rpath, att).getOrElse(throw new IllegalArgumentException("No attribute "+att+" in view "+view)).asInstanceOf[Object]
    }.toMap
    
    readAttributes.asJava
  }
  override def newByteChannel(path: Path, options: JSet[_ <: OpenOption], attrs: FileAttribute[_]*): SeekableByteChannel = {
    val rPath = ramPath(path)
    val opts = options.asScala.asInstanceOf[Set[OpenOption]]
    val node = rPath.getFileSystem.lookup(rPath) getOrElse {
        import scalax.io.{StandardOpenOption => SOO}
        val createOpts = Set(SOO.Create, SOO.CreateNew, SOO.CreateFull)
        if ( opts.exists {opt => createOpts.contains(opt)} ) {
         
          rPath.getFileSystem.create(rPath, FileNode, opts.contains(SOO.CreateFull), toMap(attrs))
          
        } else {
          throw new NoSuchFileException(path+" does not exist")
        }
    }
    node match {
      case fNode: FileNode =>
        fNode.channel(opts.toSeq:_*)
      case _ => throw new NoSuchFileException(path+" is not a file it is a directory")
    }
  }
  override def setAttribute(path: Path, attribute: String, value: Any, options: LinkOption*): Unit = {
   // TODO ;jaslf 
  }
  override def getFileAttributeView[V <: FileAttributeView](path: Path, typeOf: Class[V], options: LinkOption*): V = {
    val ramP = ramPath(path)
    if(!ramP.exists) throw new FileNotFoundException(path+" Does not exist")

    val Basic = classOf[BasicFileAttributeView]

    typeOf match {
      case Basic => new RamBasicFileView(ramP).asInstanceOf[V]
      case _ => null.asInstanceOf[V]
    }
  }
  override def readAttributes[A <: BasicFileAttributes](path: Path, typeOf: Class[A], options: LinkOption*): A = {
    val ramP = ramPath(path)
    if(!ramP.exists) throw new FileNotFoundException(path+" Does not exist")

    val Basic = classOf[BasicFileAttributes]

    typeOf match {
      case Basic => new BasicRamFileAttributes(ramP).asInstanceOf[A]
      case _ => null.asInstanceOf[A]
    }
  }
  override def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[_ >: Path]): DirectoryStream[Path] = 
    new RamDirectoryStream(ramPath(dir), filter)

  // -------------  Support methods --------------------//
  def readAttribute(path: RamPath, att: RamAttributes.RamAttribute) = {
    path.getFileSystem.lookup(path) flatMap (_.attributes.get(att))
  }
  def ramPath(p:Path):RamPath = p match {
    case rp: RamPath => rp
    case _ => throw new IllegalArgumentException(p.getClass+" is not a RamPath instance")
  }
 
  def ramPath(paths:Path*):Seq[RamPath] = {
    val rps = paths.map(ramPath(_))
    val fs = rps.head.getFileSystem
    if(!(rps forall {_.getFileSystem == fs})) {
      throw new IllegalArgumentException(paths+" must all have the same filesystem")
    }
    rps
  }
 
  def toMap(attrs: Seq[FileAttribute[_]]) = {
     val attMap = attrs.map { att =>
        val ramAtt = RamAttributes.values.find(_.toString == att.name()) getOrElse (throw new IllegalArgumentException(att.name+" is not an attribute that is supported by this file system"))
        ramAtt -> att.value.asInstanceOf[Object]
      }
     attMap.toMap
  }
}