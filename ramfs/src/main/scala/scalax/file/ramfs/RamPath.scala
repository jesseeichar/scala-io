package scalax.file.ramfs

import java.io.File
import java.nio.file.{ Path, FileSystem, WatchService, LinkOption, WatchEvent, WatchKey }
import WatchEvent._
import java.net.URI
import java.util.{ Iterator => JIterator }
import java.util.regex.Pattern

class RamPath(relativeTo: String, val path: String, fs: RamFileSystem) extends Path {

  val sep = fs.getSeparator
  lazy val name =  path.split(Pattern.quote(sep)).lastOption getOrElse (path)
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
  override def getFileSystem(): FileSystem = fs

  override def isAbsolute: Boolean = relativeTo == ""

  override def getRoot(): RamPath = fs.root

  override def getFileName(): RamPath = {
    val basePath = path.dropRight(name.length)
    fs.fromStrings(relativeTo+sep+basePath, name)
  }

  override def getParent(): RamPath = {
    (segments dropRight 1) match {
      case Nil => null
      case segs => fs.fromStrings(relativeTo, segments mkString sep)
    }
  }

  override def getNameCount(): Int = segments.size

  override def getName(index: Int): RamPath = fs.fromStrings((relativeTo :+ segments.take(index-1)) mkString sep, segments(index) )

  override def subpath(beginIndex: Int, endIndex: Int): RamPath = null.asInstanceOf[RamPath]

  override def startsWith(other: Path): Boolean = false

  override def startsWith(other: String): Boolean = false
  override def endsWith(other: Path): Boolean = false

  override def endsWith(other: String): Boolean = false

  override def normalize(): RamPath = null.asInstanceOf[RamPath]

  override def resolve(other: Path): RamPath = null.asInstanceOf[RamPath]

  override def resolve(other: String): RamPath = null.asInstanceOf[RamPath]

  override def resolveSibling(other: Path): RamPath = null.asInstanceOf[RamPath]

  override def resolveSibling(other: String): RamPath = null.asInstanceOf[RamPath]

  override def relativize(other: Path): RamPath = null.asInstanceOf[RamPath]

  override def toUri(): URI = null.asInstanceOf[URI]

  override def toAbsolutePath(): RamPath = null.asInstanceOf[RamPath]

  override def toRealPath(options: LinkOption*): Path = null.asInstanceOf[RamPath]

  override def toFile(): File = null.asInstanceOf[File]

  override def register(watcher: WatchService, events: Array[Kind[_]], modifiers: Modifier*): WatchKey = null.asInstanceOf[WatchKey]

  override def register(watcher: WatchService, events: Kind[_]*): WatchKey = null.asInstanceOf[WatchKey]

  override def iterator(): JIterator[Path] = null.asInstanceOf[JIterator[Path]]

  override def compareTo(other: Path): Int = 0
}