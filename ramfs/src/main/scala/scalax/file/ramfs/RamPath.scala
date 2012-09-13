package scalax.file.ramfs

import java.io.File
import java.nio.file.{ Path, FileSystem, WatchService, LinkOption, WatchEvent, WatchKey }
import WatchEvent._
import java.net.URI
import java.util.{ Iterator => JIterator }
import java.util.regex.Pattern

class RamPath(protected[ramfs] val segments: Vector[String], fs: RamFileSystem) extends Path {
  def exists = fs.lookup(this).isDefined
  val sep = fs.getSeparator
  lazy val name = segments.last
  val path = segments mkString sep
  override def getFileSystem(): RamFileSystem = fs

  override def isAbsolute: Boolean = segments.startsWith(fs.root.segments)

  override def getRoot(): RamPath = fs.root

  override def getFileName(): RamPath = new RamPath(Vector(name), fs)

  override def getParent(): RamPath = if (segments.size > 1) {
   new RamPath(segments.dropRight(1), fs) 
  } else {
    null
  }

  override def getNameCount(): Int = segments.size

  override def getName(index: Int): RamPath = new RamPath(Vector(segments(index)), fs)

  override def subpath(beginIndex: Int, endIndex: Int): RamPath = {
    new RamPath(segments.slice(beginIndex, endIndex), fs)
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
    new RamPath(reversedNormalizedPath.reverse, fs)
  }

  override def resolve(other: Path): RamPath = other match {
    case ramPath: RamPath if ramPath.getFileSystem == fs =>
      new RamPath(segments ++ ramPath.segments, fs)
    case _ => throw new java.io.IOException("Can only resolve with paths in the same filesystem")
  }

  override def resolve(other: String): RamPath = {
    fs.fromStrings(path + sep + other)
  }

  override def resolveSibling(other: Path): RamPath = other match {
    case ramPath: RamPath if ramPath.getFileSystem == fs =>
      val newSegments = 
        if(segments.nonEmpty) segments.dropRight(1) ++ ramPath.segments
        else ramPath.segments
      new RamPath(newSegments, fs)
    case _ => throw new java.io.IOException("Can only resolve with paths in the same filesystem")
  }

  override def resolveSibling(other: String): RamPath = resolveSibling(fs.getPath(other))

  override def relativize(other: Path): RamPath = other match {
    case ramPath: RamPath if ramPath.getFileSystem == fs && ramPath.segments.startsWith(segments)=>
      val newSegments = ramPath.segments.drop(segments.size)
      new RamPath(newSegments, fs)
    case _ => throw new java.io.IOException("Can only relativize with paths in the same filesystem")
  }

  override def toUri(): URI = {
    new URI((fs.provider.getScheme())+"://"+(fs.id)+"@"+(path.replaceAll("\\\\","/")))
  }

  override def toAbsolutePath(): RamPath = this

  override def toRealPath(options: LinkOption*): Path = this

  override def toFile(): File = null.asInstanceOf[File]

  override def iterator(): JIterator[Path] = new JIterator[Path]{
    var index = 0
    def hasNext = index < segments.size
    def next = {
      if (!hasNext) throw new NoSuchElementException()
      index += 1
	  new RamPath(segments.take(index), fs)
    }
    def remove = throw new UnsupportedOperationException()
  }

  override def compareTo(other: Path): Int = toAbsolutePath.path.compareTo(other.toAbsolutePath.toString)

  override def toString = path
  
  override def register(watcher: WatchService, events: Array[Kind[_]], modifiers: Modifier*): WatchKey = null.asInstanceOf[WatchKey]

  override def register(watcher: WatchService, events: Kind[_]*): WatchKey = null.asInstanceOf[WatchKey]
}