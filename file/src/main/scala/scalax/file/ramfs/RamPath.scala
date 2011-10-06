/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package ramfs
import Path.AccessModes._
import Path.fail

import java.net.URI

import java.util.regex.Pattern

class RamPath(relativeTo: String, val path: String, override val fileSystem: RamFileSystem) extends Path(fileSystem) with RamFileOps {
  def node = fileSystem.lookup(this)

  lazy val toAbsolute: RamPath = fileSystem.fromStrings("", relativeTo + separator + path)
  lazy val toURI: URI = fileSystem.uri(this)

  def /(child: String): RamPath = fileSystem.fromStrings(relativeTo, path + separator + child)

  lazy val name: String = path.split(Pattern.quote(separator)).lastOption getOrElse (path)
  override lazy val normalize = super.normalize.asInstanceOf[RamPath]
  lazy val parent: Option[RamPath] = {
    val segs = {
      val raw = path.split(Pattern quote separator).toList
      val noEmpty = raw.filterNot {
        _.isEmpty
      }
      if (raw.headOption.exists {
        _.isEmpty
      }) separator +: noEmpty
      else noEmpty
    };
    (segs dropRight 1) match {
      case Nil => None
      case segs => Some(fileSystem(relativeTo, segs))
    }
  }

  def checkAccess(modes: AccessMode*): Boolean = node match {
    case None =>
      false
    case Some(node) =>
      modes forall {
        case Execute => node.canExecute
        case Read => node.canRead
        case Write => node.canWrite
      }
  }

  def exists = node.isDefined

  def isFile = node.map(FileNode.accepts).getOrElse(false)

  def isDirectory = node.map(DirNode.accepts).getOrElse(false)

  def isAbsolute = relativeTo == ""

  def isHidden = false

  //TODO
  def lastModified = node map {
    _.lastModified
  } getOrElse 0

  def lastModified_=(time: Long) = {
    node foreach {
      _.lastModified = time
    }
    time
  }

  def size = node collect {
    case f: FileNode => f.data.size.toLong
  }

  def access_=(accessModes: Iterable[AccessMode]) = node match {
    case None => fail("Path %s does not exist".format(path))
    case Some(node) =>
      node.canRead = accessModes exists {
        _ == Read
      }
      node.canWrite = accessModes exists {
        _ == Write
      }
      node.canExecute = accessModes exists {
        _ == Execute
      }
  }

  def doCreateFile(): Boolean = fileSystem.create(this, FileNode, false)

  def doCreateDirectory(): Boolean = fileSystem.create(this, DirNode, false)

  def doCreateParents(): Unit = this.toAbsolute.parent.foreach(fileSystem.create(_, DirNode, true))

  def delete(force: Boolean): Path = {
    if (node.collect {
      case d: DirNode => d.children.isEmpty
    }.forall {
      p => p
    }) {
      if (exists && !fileSystem.delete(this, force)) {
        fail("Could not delete " + path)
      }
    } else {
      fail("Directory is not empty, cannot delete")
    }
    this
  }

  protected def moveFile(target: Path, atomicMove: Boolean): Unit = fileSystem.move(this, target.asInstanceOf[RamPath])

  protected def moveDirectory(target: Path, atomicMove: Boolean): Unit = fileSystem.move(this, target.asInstanceOf[RamPath])

  override def toString() = "RamPath(%s)".format(path)

  def descendants[U >: Path, F](filter: F, depth: Int, options: Traversable[LinkOption])(implicit factory: PathMatcherFactory[F]) = {
    if (!isDirectory) throw new NotDirectoryException(this + " is not a directory so descendants can not be called on it")
    
    new BasicPathSet[RamPath](this, factory(filter), depth, false, (pathFilter: PathMatcher, parent: RamPath) => {
      parent.node.collect {
        case d: DirNode =>
          d.children.map(n => parent / n.name)
        case p =>
          throw new NotDirectoryException(p+" is not a directory so descendants can not be called on it")
      }.flatten.toIterator
    })
  }

}
