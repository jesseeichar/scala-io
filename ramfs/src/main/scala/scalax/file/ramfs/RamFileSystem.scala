package scalax.file.ramfs

import java.io.IOException
import language.reflectiveCalls
import java.nio.file.{ FileSystem, PathMatcher, Path, FileStore, WatchService, Files, NoSuchFileException }
import java.nio.file.attribute.{UserPrincipalLookupService, FileAttribute}
import java.nio.file.spi.FileSystemProvider
import java.lang.{ Iterable => JIterable }
import java.util.{ UUID, Set => JSet }
import java.net.URI
import scalax.file.ImplicitConverters._
import scalax.file.PathMatcher.{ StandardSyntax, RegexPathMatcher, GlobPathMatcher }
import collection.JavaConverters._
import java.nio.file.AccessMode

case class RamFsId(id: String = UUID.randomUUID.toString)

object RamFileSystem {
  val provider = new RamFsProvider()
  private val fileSystems = scala.collection.mutable.WeakHashMap[RamFsId, RamFileSystem]()
  def existsFileSystem(uri: URI) = fileSystems.contains(RamFsId(uri.getUserInfo))
  def apply(separator: String = "/"): RamFileSystem = new RamFileSystem(separator = separator)()
  def apply(fsId: RamFsId): RamFileSystem = synchronized {
    fileSystems.get(fsId).getOrElse(new RamFileSystem(fsId)())
  }
  def apply(uri: URI): RamPath = {
    require(uri.toString contains '@', "Ramfile system URIs must be of form: ramfs://fsId@path, was: " + uri + " (did not contain a !)")
    require(uri.getScheme equalsIgnoreCase "ramfs", "Ramfile system URIs must start with ramfs, was: " + uri)

    val id = RamFsId(uri.getUserInfo)
    val fs = apply(id)
    val path = uri.getRawPath split "/"
    val relativeTo = if (path(0) == fs.root.path) "" else fs.pwd.toAbsolutePath.toString
    fs.fromStrings(relativeTo, path mkString fs.getSeparator)
  }
  private def register(fsId: RamFsId, fs: RamFileSystem) = synchronized {
    fileSystems(fsId) = fs
  }
}
class RamFileSystem(val id: RamFsId = RamFsId(), separator: String = "/", workingDir: String = "")(fileStore: RamFileStore = new RamFileStore(id)) extends FileSystem {
  RamFileSystem.register(id, this)

  private var fsTree = new DirNode(separator)
  val root = new RamPath("", fsTree.name, this)
  val pwd = fromStrings("", workingDir)

  override def provider: FileSystemProvider = RamFileSystem.provider
  override def close: Unit = ()
  override def isOpen = true
  override def isReadOnly = false
  override def getSeparator = separator

  override val getRootDirectories = new JIterable[Path] {
    def iterator = new java.util.Iterator[Path] {
      var done = false
      def hasNext = !done
      def next = {
        if (!hasNext) throw new NoSuchElementException()
        done = true
        root
      }
      def remove = throw new java.lang.UnsupportedOperationException()
    }
  }

  protected[ramfs] def fromStrings(relativeTo: String, path: String): RamPath = {
    def process(path: String) = {
      import java.util.regex.Pattern.quote
      val p = path.replace(separator + separator, separator);
      if ((p endsWith separator) && (p.length > 1)) p.drop(1)
      else p
    }
    val newpath = new RamPath(process(relativeTo), process(path), this)
    if (newpath == root) root
    else newpath
  }
  override def getPath(first: String, more: String*): RamPath = fromStrings(workingDir, (first +: more).filterNot { _.isEmpty } mkString separator)

  override def getPathMatcher(syntaxAndPattern: String): PathMatcher = {
    val Array(syntax, pattern) = syntaxAndPattern.split(":", 2)
    syntax match {
      case StandardSyntax.GLOB => GlobPathMatcher(pattern).asJavaPathMatcher
      case StandardSyntax.REGEX => RegexPathMatcher(pattern).asJavaPathMatcher
      case _ => throw new UnsupportedOperationException(syntax + " is not a recognized syntax for the RamFileSystem filesystem")
    }
  }

  override def getFileStores: JIterable[FileStore] = {
    Iterable(fileStore: FileStore).asJava
  }

  override def supportedFileAttributeViews: JSet[String] = fileStore.supportedViewNames.toSet.asJava

  override def getUserPrincipalLookupService: UserPrincipalLookupService = null.asInstanceOf[UserPrincipalLookupService]

  override def newWatchService: WatchService = null.asInstanceOf[WatchService]

  // ----------------- Support Methods --------------------//
  private[ramfs] def lookup(path: RamPath) = {
    val absolutePath = path.toAbsolutePath.segments
    fsTree.lookup(absolutePath)
  }

  private[ramfs] def create(path: RamPath, fac: NodeFac, createParents: Boolean, attrs:Map[RamAttributes.RamAttribute,Object]): Node = {
    if (path == root) {
      fsTree
    } else {
      val absolute = path.toAbsolutePath
      Option(absolute.getParent) match {
        case Some(p) if !p.exists && !createParents =>
          throw new java.io.FileNotFoundException("Parent directory " + p + " does not exist")
        case _ => ()
      }

      val x = fsTree.create(absolute.segments.drop(1), fac)
      x.attributes ++= attrs
      x
    }
  }
  private[ramfs] def delete(path: RamPath, force: Boolean): Boolean = {
    if (path.exists) {
      def delete(p: Path) = force || (Files.isWritable(p) && Option(p.getParent).forall { Files.isWritable })

      if (delete(path) && path != root) {
        val parentPath = Option(path.toAbsolutePath.getParent)
        val deletions = for {
          parent <- Option(path.toAbsolutePath.getParent)
          parentNode <- lookup(parent)
          node <- lookup(path)
        } yield {
          parentNode.asInstanceOf[DirNode].children -= node
          true
        }
        deletions.isDefined
      } else if (path == root) {
        fsTree = new DirNode(separator)
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  private[ramfs] def move(src: RamPath, dest: RamPath) = {
    if (src == root) {
      throw new java.io.IOException("Root cannot be moved")
    }
    val parentNode =
      Option(dest.getParent) match {
        case Some(`root`) | None =>
          fsTree
        case Some(parent) =>
          create(parent, DirNode, true, Map.empty) // TODO paramaterize NodeFactory
          lookup(parent).get.asInstanceOf[DirNode]
      }

    lookup(src) foreach { node =>
      node.name = dest.name
      parentNode.children += node
    }

    delete(src, true)
  }

  /**
   * creates and copies the data of the src node to the destination.
   * Assumption is the destination does not exist
   */
  private[ramfs] def copyFile(src: RamPath, dest: RamPath) = {
    val srcNode = {
      val node = lookup(src) getOrElse (throw new NoSuchFileException(src+" does not exist"))
      if(!node.isInstanceOf[FileNode]) throw new IOException("Path does not reference a file")
      node.asInstanceOf[FileNode]
    }
    dest.getFileSystem.create(dest, FileNode, true, Map.empty)
    val newNode = lookup(dest).collect {
      case newNode: FileNode =>
        newNode.data.clear
        newNode.data ++= srcNode.data
    }
  }
}