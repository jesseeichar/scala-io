/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.defaultfs

import _root_.resource.ManagedResource
import scalax.io.attributes.FileAttribute
import java.io.{
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, 
  BufferedInputStream, BufferedOutputStream, IOException, File => JFile}
import java.net.{ URI, URL }
import collection.{Traversable }
import PartialFunction._
import util.Random.nextPrintableChar
import java.lang.{ProcessBuilder}

import scalax.io._
import Path._
import Path.AccessModes._


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
//private[io] 
class DefaultPath private[io] (val jfile: JFile, override val fileSystem: DefaultFileSystem) extends Path(fileSystem) with DefaultFileOps
{
  self =>

  def toAbsolute: Path = if (isAbsolute) this else Path(jfile.getAbsolutePath())(fileSystem)
  def toURI: URI = jfile.toURI()
  def /(child: String): DefaultPath = fileSystem(new JFile(jfile, child)) // TODO check if directory is absolute
  def name: String = jfile.getName()
  def path: String = jfile.getPath()
  override lazy val normalize = super.normalize.asInstanceOf[DefaultPath]  
  def parent: Option[DefaultPath] = Option(jfile.getParent()) map fileSystem.apply
  def checkAccess(modes: AccessMode*): Boolean = {
    modes forall {
      case Execute  => jfile.canExecute() 
      case Read     => jfile.canRead()
      case Write    => jfile.canWrite()
    }
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

  def doCreateParents() = jfile.getAbsoluteFile.getParentFile.mkdirs()
  def doCreateDirectory() = jfile.getAbsoluteFile.mkdir()
  def doCreateFile() = jfile.createNewFile()
  
  def delete(force : Boolean): Path = {
    if(exists) {
      if (force) access_= (access + Write)
      
      if(!canWrite) fail("File is not writeable so the file cannot be deleted")
      if(!jfile.delete) fail("Unable to delete file for unknown reason")
    } 
    this
  }

  protected def copyFile(dest: Path): Path = {
    val FIFTY_MB = 1024 * 1024 * 50   // TODO extract this out into a constants file for easy adjustment
    assert(isFile, "Source %s is not a valid file." format name)

// TODO ARM this
    import scalax.io.OpenOption._
    for {inResource <- fileChannel()
         in <- inResource
         out <- dest.channel(Create, Truncate, Write)
    } {
      var pos, count = 0L
      while (size exists {pos < _}) {
        count = (size.get - pos) min FIFTY_MB
        val prepos = pos
        pos += in.transferTo(pos, count, out)
        if(prepos == pos) fail("no data can be copied for unknown reason!")
      }
      if (this.size != dest.size)
        fail("Failed to completely copy %s to %s".format(name, dest.name))

    }
    dest
  }
  
  protected def moveFile(target: Path, atomicMove:Boolean) : Unit = {
    target match {
      case target : DefaultPath if jfile renameTo target.jfile => 
        () // moved worked as part of guard
      case _ =>
        target write this.bytesAsInts
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
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }  
  override def hashCode() = path.hashCode()

  def descendants[U >: Path, F](filter:F, depth:Int, options:Traversable[LinkOption])(implicit factory:PathMatcherFactory[F]) = {
    new BasicPathSet[DefaultPath](this, factory(filter), depth, false, (_:DefaultPath).jfile.listFiles.view.map (fileSystem.apply).toList)
  }


}
