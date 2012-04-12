/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package defaultfs


import java.io.{
  File => JFile}
import java.net.URI
import collection.Traversable
import scalax.io._
import Path._
import Path.AccessModes._
import java.io.FileFilter
import scala.annotation.tailrec
import java.util.regex.Pattern


/**
 * <b>Not Part of API</b>
 * <p>
 * A file reference that locates a file using a system independent path.
 * The file is not required to exist.
 * </p>
 *  @author  Paul Phillips
 *  @author  Jesse Eichar
 *  @since   1.0
 */
//private[file]
class DefaultPath private[file] (val jfile: JFile, override val fileSystem: DefaultFileSystem) extends Path(fileSystem) with DefaultFileOps
{
  self =>

  def toAbsolute: DefaultPath = if (isAbsolute) this else fileSystem.fromString(jfile.getAbsolutePath())

  def toURI: URI = jfile.toURI()
  def /(child: String): DefaultPath = {
    fileSystem.checkSegmentForSeparators(child)
    fileSystem(new JFile(jfile, child))
  }
  def name: String = jfile.getName()
  def path: String = jfile.getPath()
  def toRealPath(linkOptions:LinkOption*) = new DefaultPath(jfile.getCanonicalFile(), fileSystem)
  override def toFile:Option[java.io.File] = Some(jfile)
  def parent: Option[DefaultPath] = Option(jfile.getParentFile()) map (jf => new DefaultPath(jf,fileSystem))
  def checkAccess(modes: AccessMode*): Boolean = {
    modes forall {
      case Execute  => jfile.canExecute()
      case Read     => jfile.canRead()
      case Write    => jfile.canWrite()
    }
  }
  private[this] val sepRegex = Pattern.compile(Pattern.quote(separator)+"+")
  override lazy val segments = {
    @tailrec
    def fileSegments(f:JFile, acc:Seq[String]):Seq[String] = {
      val parent = f.getParentFile()
      if (parent == null) {
        // these shenanigans are because windows can have \ as the root path separator
        // in this we need that separator so we do this madness (getName()) returns the empty string
        // so we can't simply call getName
//        (if (sepRegex.matcher(f.getPath()).find()) separator else f.getPath) +: acc
        val root = f.getPath().filterNot(_ == separator(0))
        (if (root.isEmpty) separator else root) +: acc
      } else {
        fileSegments(parent, f.getName +: acc)
      }
    }
    fileSegments(jfile, Vector.empty[String])
  }
  override def canWrite  = jfile.canWrite
  override def canRead = jfile.canRead
  override def canExecute = jfile.canExecute
  def exists = jfile.exists()
  override def nonExistent = try !jfile.exists() catch { case ex: SecurityException => false }
  def isFile = jfile.isFile()
  def isDirectory = jfile.isDirectory()
  def isAbsolute = jfile.isAbsolute()
  def isHidden = jfile.isHidden()
  def lastModified = jfile.lastModified()
  def lastModified_=(time: Long) = {jfile setLastModified time; time}
  def size = if(jfile.exists) Some(jfile.length()) else None

  def access_=(accessModes:Iterable[AccessMode]) = {
    if (nonExistent) fail("Path %s does not exist".format(path))

    jfile.setReadable(accessModes exists {_==Read})
    jfile.setWritable(accessModes exists {_==Write})
    jfile.setExecutable(accessModes exists {_==Execute})
  }

  def doCreateParents() = Option(jfile.getAbsoluteFile.getParentFile).map(_.mkdirs())
  def doCreateDirectory() = jfile.getAbsoluteFile.mkdir()
  def doCreateFile() = jfile.createNewFile()

  def delete(force : Boolean): this.type = {
    if(exists) {
      if (force) access_= (access + Write)

      if(!canWrite) fail("File is not writeable so the file cannot be deleted")
      if(!jfile.delete) {
        if(children().nonEmpty) fail("use deleteRecursively if you want to delete directory and descendants")
        else fail("Unable to delete file for unknown reason")
      }
    }
    this
  }

  protected def moveFile(target: Path, atomicMove:Boolean) : Unit = {
    target match {
      case target : DefaultPath if jfile renameTo target.jfile =>
        () // moved worked as part of guard
      case _ =>
        copyDataTo(target)
        delete()
    }
  }

  protected def moveDirectory(target:Path, atomicMove : Boolean) : Unit = {
    val y = target.exists
    target match {
      case target : DefaultPath if (jfile renameTo target.jfile) =>
        () // moved worked as part of guard
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
  def descendants[U >: Path, F](filter:F, depth:Int, options:Traversable[LinkOption])(implicit factory:PathMatcherFactory[F]) = {
    if (!isDirectory) throw new NotDirectoryException(this + " is not a directory so descendants can not be called on it")

    new BasicPathSet[DefaultPath](this, factory(filter), depth, false, { (p:PathMatcher[DefaultPath], path:DefaultPath) =>
      val files = path.jfile.listFiles
      if(files == null) Iterator.empty
      else files.toIterator.map (fileSystem.apply)
    })
  }
}
