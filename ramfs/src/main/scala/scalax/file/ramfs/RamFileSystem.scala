package scalax.file.ramfs

import language.reflectiveCalls
import java.nio.file.{ FileSystem, PathMatcher, Path, FileStore, WatchService }
import java.nio.file.attribute.UserPrincipalLookupService
import java.nio.file.spi.FileSystemProvider
import java.lang.{ Iterable => JIterable }
import java.util.{ UUID, Set => JSet }
import java.net.URI
import scalax.file.ImplicitConverters._
import scalax.file.PathMatcher.{StandardSyntax, RegexPathMatcher, GlobPathMatcher}

case class RamFsId(id: String = UUID.randomUUID.toString)

object RamFileSystem {
  val provider = new RamFsProvider()
  private val fileSystems = scala.collection.mutable.WeakHashMap[RamFsId, RamFileSystem]()
  def existsFileSystem(uri: URI) = fileSystems.contains(RamFsId(uri.getUserInfo))
  def apply(separator: String = "/"): RamFileSystem = new RamFileSystem(separator = separator)
  def apply(fsId: RamFsId): RamFileSystem = synchronized {
    fileSystems.get(fsId).getOrElse(new RamFileSystem(fsId))
  }
  def apply(uri: URI): RamPath = null.asInstanceOf[RamPath]

  //    {
  //      require(uri.toString contains '!', "Ramfile system URIs must be of form: ramfs://fsId!path, was: "+uri+" (did not contain a !)")
  //      require(uri.getScheme equalsIgnoreCase "ramfs", "Ramfile system URIs must start with ramfs, was: "+uri)
  //  
  //      val id = RamFsId(uri.getAuthority.takeWhile{_ != '!'})
  //      val fs = apply(id)
  //      val path = uri.getRawPath.replace("/", fs.separator)
  //      fs.fromString(path)
  //    }
  private def register(fsId: RamFsId, fs: RamFileSystem) = synchronized {
    fileSystems(fsId) = fs
  }
}
class RamFileSystem(id: RamFsId = RamFsId(), separator: String = "/", workingDir: String = "") extends FileSystem {
  RamFileSystem.register(id,this)

  private var fsTree = new DirNode(separator)
  val root = new RamPath("",fsTree.name, this)
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
      def remove = throw new java.lang.UnsupportedOperationException
    }
  }

protected[ramfs] def fromStrings(relativeTo:String , path: String): RamPath = {
    def process(path:String) = {
      import java.util.regex.Pattern.quote
      val p = path.replace(separator+separator, separator);
      if((p endsWith separator) && (p.length > 1)) p.drop(1)
      else p
    }
    val newpath = new RamPath(process(relativeTo), process(path), this)
    if(newpath == root) root
    else newpath
  }
  override def getPath(first: String, more: String*): RamPath = fromStrings(workingDir, (first +: more).filterNot{_.isEmpty} mkString separator)

  override def getPathMatcher(syntaxAndPattern: String): PathMatcher = {
    val Array(syntax, pattern) = syntaxAndPattern.split(":",2) 
    syntax match {
          case StandardSyntax.GLOB => GlobPathMatcher(pattern).asJavaPathMatcher
          case StandardSyntax.REGEX => RegexPathMatcher(pattern).asJavaPathMatcher
          case _ => throw new UnsupportedOperationException(syntax + " is not a recognized syntax for the RamFileSystem filesystem")
        }
  }

  override def getFileStores: JIterable[FileStore] = null.asInstanceOf[JIterable[FileStore]]

  override def supportedFileAttributeViews: JSet[String] = null.asInstanceOf[JSet[String]]
  
  override def getUserPrincipalLookupService: UserPrincipalLookupService = null.asInstanceOf[UserPrincipalLookupService]

  override def newWatchService: WatchService = null.asInstanceOf[WatchService]
}