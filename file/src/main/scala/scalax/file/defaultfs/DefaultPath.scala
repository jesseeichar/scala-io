/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package defaultfs


import java.nio.file.{Path => JPath, Files => JFiles}
import java.net.URI
import collection.Traversable
import scalax.io._
import Path._
import Path.AccessModes._
import java.io.FileFilter
import scala.annotation.tailrec
import java.util.regex.Pattern
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.attribute.{
  FileTime, PosixFilePermission
}
import java.nio.file.StandardCopyOption


/**
 * <b>Not Part of API</b>
 * <p>
 * A file reference that locates a file using a system independent path.
 * The file is not required to exist.
 * </p>
 *  @author  Jesse Eichar
 *  @since   1.0
 */
//private[file]
class DefaultPath private[file] (val jfile: JPath, override val fileSystem: DefaultFileSystem) extends Path(fileSystem) with DefaultFileOps
{
  self =>

  override def toAbsolute: DefaultPath = if (isAbsolute) this else new DefaultPath(jfile.toAbsolutePath, fileSystem)

  override def toURI: URI = jfile.toUri
  override def /(child: String): DefaultPath = {
    fileSystem.checkSegmentForSeparators(child)
    fileSystem(jfile.resolve(child))
  }
  override def name: String = jfile.getFileName.toString
  override def path: String = jfile.toString
  override def toRealPath(linkOptions:LinkOption*) = new DefaultPath(jfile.toRealPath(linkOptions.map(_.toJavaLinkOptions):_*), fileSystem)
  override def fileOption:Option[java.io.File] = Option(jfile.toFile)
  override def parent: Option[DefaultPath] = Option(jfile.getParent()) map (jf => new DefaultPath(jf,fileSystem))
  override def checkAccess(modes: AccessMode*): Boolean = {
    modes forall {
      case Execute  => JFiles.isExecutable(jfile)
      case Read     => JFiles.isReadable(jfile)
      case Write    => JFiles.isWritable(jfile)
    }
  }
  private[this] val sepRegex = Pattern.compile(Pattern.quote(separator)+"+")
  override lazy val segments = new Seq[String] {
    override def length = jfile.getNameCount
    override def apply(i: Int) = jfile.getName(i).toString
    override def iterator = new Iterator[String]{
      var i = 0
      def hasNext = i < jfile.getNameCount
      def next = {
        i += 1
        jfile.getName(i-1).toString
      }
    }
  }
  override def canWrite  = JFiles.isWritable(jfile)
  override def canRead = JFiles.isReadable(jfile)
  override def canExecute = JFiles.isExecutable(jfile)
  // TODO LinkOptions
  override def exists = JFiles.exists(jfile)
  // TODO LinkOptions
  override def nonExistent = JFiles.notExists(jfile)
  // TODO LinkOptions
  override def isFile = JFiles.isRegularFile(jfile)
  // TODO LinkOptions
  override def isDirectory = JFiles.isDirectory(jfile)
  override def isAbsolute = jfile.isAbsolute()
  override def isHidden = JFiles.isHidden(jfile)
  override def lastModified = JFiles.getLastModifiedTime(jfile).toMillis
  override def lastModified_=(time: Long) = {JFiles.setLastModifiedTime(jfile, FileTime.fromMillis(time)); time}
  override def size: Option[Long] = if(exists) Some(JFiles.size(jfile)) else None

  override def access_=(accessModes:Iterable[AccessMode]) = {
    if (nonExistent) fail("Path %s does not exist".format(path))
    val permissions = accessModes.flatMap {
      case Write => PosixFilePermission.GROUP_WRITE :: PosixFilePermission.OTHERS_WRITE :: PosixFilePermission.OWNER_WRITE :: Nil
      case Read => PosixFilePermission.GROUP_READ :: PosixFilePermission.OTHERS_READ :: PosixFilePermission.OWNER_READ :: Nil
      case Execute => PosixFilePermission.GROUP_EXECUTE :: PosixFilePermission.OTHERS_EXECUTE :: PosixFilePermission.OWNER_EXECUTE:: Nil
      case _ => Nil
    }
    import collection.JavaConverters._
    JFiles.setPosixFilePermissions(jfile, permissions.toSet.asJava)
  }
// TODO Exceptions
// TODO FileAttributes
  override def doCreateParents() = Option(jfile.toAbsolutePath.getParent()).foreach(f => JFiles.createDirectories(f))
// TODO Exceptions
// TODO FileAttributes
  override def doCreateDirectory() = JFiles.createDirectory(jfile.toAbsolutePath)
// TODO Exceptions
// TODO FileAttributes
  override def doCreateFile() = JFiles.createDirectory(jfile.toAbsolutePath)

  override def delete(force : Boolean): this.type = {
    if(exists) {
      if (force) access_= (access + Write)

      if(!canWrite) fail("File is not writeable so the file cannot be deleted")
      JFiles.delete(jfile)
    }
    this
  }

  // TODO Full copy options
  override protected def moveFile(target: Path, atomicMove:Boolean) : Unit = {
    val copyOptions = if (atomicMove) Seq(java.nio.file.StandardCopyOption.ATOMIC_MOVE) else Nil
    target match {
      case target: DefaultPath =>
        JFiles.move(jfile, target.jfile, copyOptions:_*)
      case _ =>
        copyDataTo(target)
        delete()
    }
  }

  // TODO Full copy options
  override protected def moveDirectory(target:Path, atomicMove : Boolean) : Unit = {
    val copyOptions = if (atomicMove) Seq(java.nio.file.StandardCopyOption.ATOMIC_MOVE) else Nil
    val y = target.exists
    target match {
      case target: DefaultPath =>
        JFiles.move(jfile, target.jfile, copyOptions:_*)
      case _ =>
        val x = target.exists
        target.createDirectory()
        val z = descendants() forall {_.exists}
        children() foreach { path =>
          path moveTo (target \ path.relativize(self))
        }
        delete()
    }
  }

  override def toString() = "Path(%s)".format(path)
  override def descendants[U >: Path, F](filter:F, depth:Int, options:Traversable[LinkOption])(implicit factory:PathMatcherFactory[F]) = {
    if (!isDirectory) throw new NotDirectoryException(this + " is not a directory so descendants can not be called on it")

    new BasicPathSet[DefaultPath](this, factory(filter), depth, false, { (p:PathMatcher[DefaultPath], path:DefaultPath) =>
      // TODO Native filters
      new CloseableIterator[DefaultPath] {
        val stream = JFiles.newDirectoryStream(path.jfile)
        val iter = stream.iterator
        def doClose = try {stream.close; Nil} catch {case e:Throwable => List(e)}
        def hasNext = iter.hasNext
        def next = {
          // TODO LinkOptions
          new DefaultPath(iter.next, fileSystem)
        }
      }
    })
  }
}

