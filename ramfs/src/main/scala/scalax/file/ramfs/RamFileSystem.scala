package scalax.file.ramfs

import java.io.IOException
import language.reflectiveCalls
import java.nio.file.{ FileSystem, PathMatcher, Path, FileStore, WatchService, Files, NoSuchFileException }
import java.nio.file.attribute.{UserPrincipalLookupService, FileAttribute}
import java.nio.file.spi.FileSystemProvider
import java.lang.{ Iterable => JIterable }
import java.util.{ UUID, Set => JSet }
import java.net.{URI, URLDecoder, URLEncoder}
import scalax.file.ImplicitConverters._
import scalax.file.PathMatcher.{ StandardSyntax, RegexPathMatcher, GlobPathMatcher }
import collection.JavaConverters._
import java.nio.file.AccessMode
import java.util.regex.Pattern
import actor._

case class RamFsId(id: String = UUID.randomUUID.toString)

object RamFileSystem {
  
  val provider = new RamFsProvider()
  def createURI(id:String, separator:String) = new URI((provider.getScheme+"://%s:%s@ramfs/").format(id, URLEncoder.encode(separator, "UTF8")))
  
  private val fileSystems = scala.collection.mutable.WeakHashMap[RamFsId, RamFileSystem]()
  def existsFileSystem(uri: URI) = fileSystems.contains(RamFsId(uri.getUserInfo))
  def apply(fsId: RamFsId, separator:String = "/"): RamFileSystem = synchronized {
    fileSystems.get(fsId).getOrElse(new RamFileSystem(fsId, separator)())
  }
  def apply(uri:URI): RamFileSystem = {
    require(uri.toString contains '@', "Ramfile system URIs must be of form: ramfs://fsId@path, was: " + uri + " (did not contain a !)")
    require(uri.getScheme equalsIgnoreCase "ramfs", "Ramfile system URIs must start with ramfs, was: " + uri)

    val Pattern = ("""ramfs://(.+):?(.*)@ramfs/.*""").r
    val Pattern(rawid, rawsep) = uri.getRawAuthority().split(":",2)
    val id = RamFsId(rawid)
    apply(id, rawsep)

  }
  def createPath(uri: URI): RamPath = {
    val fs = apply(uri)
    val path = uri.getRawPath split "/"
    fs.fromStrings(path mkString fs.getSeparator)
  }
  private def register(fsId: RamFsId, fs: RamFileSystem) = synchronized {
    fileSystems(fsId) = fs
  }
}
class RamFileSystem(val id: RamFsId = RamFsId(), val separator:String = "/", workingDir: String = "")(fileStore: RamFileStore = new RamFileStore(id)) extends FileSystem {
  val actor = new RamFsActor(this)
  actor.start()
  
  RamFileSystem.register(id, this)
  override def provider: FileSystemProvider = RamFileSystem.provider
  override def close: Unit = actor ! RamFsMsg.Stop
  override def isOpen = actor !? RamFsMsg.IsRunning match {
    case RamFsResponse.Stopped => false
    case RamFsResponse.Running => true
    case r => throw new IllegalStateException(r+" is not a legal response for IsRunning")
  }
  override def isReadOnly = false
  override def getSeparator = separator
  val  pwd = fromStrings(workingDir)
  val root = new RamPath(Vector(separator), this)
  
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

  protected[ramfs] def fromStrings(path: String): RamPath = {
    val cleanPath = path.split(Pattern.quote(separator)).toVector.filter(_.nonEmpty)
    val newpath = new RamPath(cleanPath, this)
    if (newpath == root) root
    else newpath
  }
  override def getPath(first: String, more: String*): RamPath = fromStrings((first +: more).filterNot { _.isEmpty } mkString separator)

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

  protected[ramfs] def lookup (path: RamPath) = actor !? RamFsMsg.Lookup(path) match {
    case RamFsResponse.Lookup(node) => node
    case r =>
      throw new IllegalStateException(r+" is not a legal response of "+RamFsMsg.Lookup(path))
  }
}