package scalax.file.ramfs

import java.io.File
import java.nio.file.{ Path, FileSystem, WatchService, LinkOption, WatchEvent, WatchKey }
import WatchEvent._
import java.net.URI
import java.util.{ Iterator => JIterator }
import java.util.regex.Pattern

class RamPath(relativeTo: String, protected[ramfs] val path: String, fs: RamFileSystem) extends Path {
  def exists = fs.lookup(this).isDefined
  val sep = fs.getSeparator
  lazy val name = path.split(Pattern.quote(sep)).lastOption getOrElse (path)
  lazy val segments = {
    val raw = path.split(Pattern quote sep).toList
    val noEmpty = raw.filterNot {
      _.isEmpty
    }
    if (raw.headOption.exists {
      _.isEmpty
    }) sep +: noEmpty
    else noEmpty
  }
  override def getFileSystem(): RamFileSystem = fs

  override def isAbsolute: Boolean = relativeTo == ""

  override def getRoot(): RamPath = fs.root

  override def getFileName(): RamPath = {
    val basePath = path.dropRight(name.length)
    fs.fromStrings(relativeTo + sep + basePath, name)
  }

  override def getParent(): RamPath = {
    (segments dropRight 1) match {
      case Nil => null
      case segs => fs.fromStrings(relativeTo, segments mkString sep)
    }
  }

  override def getNameCount(): Int = segments.size

  override def getName(index: Int): RamPath = fs.fromStrings((relativeTo :+ segments.take(index - 1)) mkString sep, segments(index))

  override def subpath(beginIndex: Int, endIndex: Int): RamPath = {
    val base = (relativeTo :+ segments.take(beginIndex - 1)) mkString sep
    val path = segments.slice(beginIndex, endIndex) mkString sep
    fs.fromStrings(base, path)
  }

  override def startsWith(other: Path): Boolean = path.startsWith(other.toString())
  override def startsWith(other: String): Boolean = path.startsWith(other)
  override def endsWith(other: Path): Boolean = path.endsWith(other.toString())
  override def endsWith(other: String): Boolean = path.endsWith(other)

  override def normalize(): RamPath = {
    val Empty = Vector.empty
    val reversedNormalizedPath = (Vector[String]() /: segments) {
      case (path, ".") => path
      case (Empty, "..") => Empty
      case (path, "..") => path dropRight 1
      case (path, seg) => path :+ seg
    }
    fs.fromStrings(relativeTo, reversedNormalizedPath mkString sep)
  }

  override def resolve(other: Path): RamPath = other match {
    case ramPath: RamPath if ramPath.getFileSystem == fs =>
      val base = relativeTo + sep + path
      fs.fromStrings(base, ramPath.toString())
    case _ => throw new java.io.IOException("Can only resolve with paths in the same filesystem")
  }

  override def resolve(other: String): RamPath = {
    val base = relativeTo + sep + path
    fs.fromStrings(base, other)
  }

  override def resolveSibling(other: Path): RamPath = other match {
    case ramPath: RamPath if ramPath.getFileSystem == fs =>
      val base = (relativeTo +: segments.dropRight(1)) mkString sep
      fs.fromStrings(base, ramPath.toString())
    case _ => throw new java.io.IOException("Can only resolve with paths in the same filesystem")
  }

  override def resolveSibling(other: String): RamPath = {
    val base = (relativeTo +: segments.dropRight(1)) mkString sep
    fs.fromStrings(base, other)
  }

  override def relativize(other: Path): RamPath = other match {
    case ramPath: RamPath if ramPath.getFileSystem == fs =>
      val base = (relativeTo +: segments.drop(ramPath.segments.size)) mkString sep
      fs.fromStrings(base, ramPath.toString())
    case _ => throw new java.io.IOException("Can only relativize with paths in the same filesystem")
  }

  override def toUri(): URI = {
    new URI((fs.provider.getScheme())+"://"+(fs.id)+"@"+(path.replaceAll("\\\\","/")))
  }

  override def toAbsolutePath(): RamPath = new RamPath("", relativeTo+sep+path, fs)

  override def toRealPath(options: LinkOption*): Path = {
    // TODO support links
    new RamPath("", relativeTo+sep+path, fs)
  }

  override def toFile(): File = null.asInstanceOf[File]

  override def iterator(): JIterator[Path] = new JIterator[Path]{
    var index = 0
    def hasNext = index < segments.size
    def next = {
      if (!hasNext) throw new NoSuchElementException()
      val base = (relativeTo +: segments.take(index)) mkString sep
      val p = segments(index)
	  index += 1
	  new RamPath(base, p, fs)
    }
    def remove = throw new UnsupportedOperationException()
  }

  override def compareTo(other: Path): Int = toAbsolutePath.path.compareTo(other.toAbsolutePath.toString)

  override def toString = path
  
  override def register(watcher: WatchService, events: Array[Kind[_]], modifiers: Modifier*): WatchKey = null.asInstanceOf[WatchKey]

  override def register(watcher: WatchService, events: Kind[_]*): WatchKey = null.asInstanceOf[WatchKey]
}