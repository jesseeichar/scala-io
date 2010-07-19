/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.defaultfs

import scala.resource.ManagedResource
import scalax.io.attributes.FileAttribute
import scalax.io.{
  Path, FileOps, PathMatcher, DirectoryStream, LinkOption
}

import java.io.{ 
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, 
  BufferedInputStream, BufferedOutputStream, IOException, File => JFile}
import java.net.{ URI, URL }
import collection.{Traversable }
import PartialFunction._
import util.Random.nextPrintableChar
import java.lang.{ProcessBuilder}

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
class DefaultPath private[io] (val jfile: JFile, override val fileSystem: DefaultFileSystem) extends Path(fileSystem)
{
  self =>
  
  def toAbsolute: Path = if (isAbsolute) this else Path(jfile.getAbsolutePath())(fileSystem)
  def toURI: URI = jfile.toURI()
  def \(child: String): DefaultPath = fileSystem(new JFile(jfile, child)) // TODO check if directory is absolute
  def name: String = jfile.getName()
  def path: String = jfile.getPath()
  def normalize: DefaultPath = fileSystem(jfile.getCanonicalPath())
  def parent: Option[DefaultPath] = Option(jfile.getParent()) map fileSystem.apply
  def checkAccess(modes: AccessMode*): Boolean = {
    modes forall {
      case Execute  => jfile.canExecute() 
      case Read     => jfile.canRead()
      case Write    => jfile.canWrite()
      case m => fail("Access mode "+m+" is not recognized as a access mode for DefaultPath")
    }
  }
  def canWrite  = jfile.canWrite
  def canRead = jfile.canRead
  def canExecute = jfile.canExecute
  def exists = jfile.exists()
  override def notExists = try !jfile.exists() catch { case ex: SecurityException => false }
  def isFile = jfile.isFile()
  def isDirectory = jfile.isDirectory()
  def isAbsolute = jfile.isAbsolute()
  def isHidden = jfile.isHidden()
  def lastModified = jfile.lastModified()
  def lastModified_=(time: Long) = {jfile setLastModified time; time}
  def size = jfile.length()
  
  def access_=(accessModes:Iterable[AccessMode]) = {
    if (notExists) fail("Path %s does not exist".format(path))

    jfile.setReadable(accessModes exists {_==Read})
    jfile.setWritable(accessModes exists {_==Write})
    jfile.setExecutable(accessModes exists {_==Execute})
  }
  def access : Set[AccessMode] = {
    AccessModes.values filter { 
      case Read => canRead
      case Write => canWrite
      case Execute => canExecute
      case e => throw new Error(AccessModes.values.mkString)
    }
  }
  
  
  private def createContainingDir(createParents: Boolean) = {
    def testWrite(parent:Option[Path]) = {
      parent match {
        case Some(p) if(!p.canWrite) => fail("Cannot write parent directory: "+p+" of "+ path)
        case Some(p) if(!p.isDirectory) => fail("parent path is not a directory")
        case Some(p) if(!p.canExecute) => fail("Cannot execute in parent directory: "+p+" of "+ path)
        case Some(p) => ()
        case None => fail("Parent directory cannot be created: '"+path+"'")
      }
    }

    (createParents, parent) match {
      case (_, Some(p)) if(p.exists) => 
        testWrite(parent)
      case (true, Some(p)) => 
        p.jfile.getAbsoluteFile.mkdirs()
        testWrite(parent)
      case (true, None) => 
        jfile.getAbsoluteFile.getParentFile.mkdirs()
        testWrite(toAbsolute.parent)
      case (false, _) => fail("Parent directory does not exist")
    }
  }
  def createFile(createParents: Boolean = true, failIfExists: Boolean = true, 
                 accessModes:Iterable[AccessMode]=List(Read,Write), attributes:Iterable[FileAttribute[_]]=Nil): Path = {
                   
    if(exists && failIfExists) {
       fail("File '%s' already exists." format name)
    } else if (exists) {
      this
    } else {
      createContainingDir (createParents)
      // next assertions should be satisfied by createContainingDir or have already thrown an exception
      assert(parent.forall{p => p.exists}, "parent must exist for a file to be created within it")
      assert(parent.forall{p => p.canWrite}, "parent must be writeable")
      assert(parent.forall{p => p.isDirectory}, "parent must be executable")
      assert(parent.forall{p => p.canExecute}, "parent must be executable")
    
      val res = jfile.createNewFile()
      access = accessModes
      if (!res) fail("unable to create file")
      else this
    }
  }
  
  def createDirectory(createParents: Boolean, failIfExists: Boolean,
                      accessModes:Iterable[AccessMode]=List(Read,Write,Execute),
                      attributes:Iterable[FileAttribute[_]]=Nil) = {
      createContainingDir (createParents)

      val res = jfile.mkdir()
      
      access = accessModes
      if (!res && failIfExists && exists) fail("Directory '%s' already exists." format name)
      else this
  }
  
  def delete(force : Boolean): Path = {
    if(exists) {
      if (force) access_= (access + Write)
      
      if(!canWrite) fail("File is not writeable so the file cannot be deleted")
      if(!jfile.delete) fail("Unable to delete file for unknown reason")
    } 
    this
  }

  def copyTo(target: Path, 
             createParents : Boolean = true, 
             copyAttributes:Boolean=true,
             replaceExisting:Boolean=false): Path = {

  	if (this.normalize == target.normalize) return target
  	
    if (!createParents && target.parent.map(_.notExists).getOrElse(true)) fail("Parent directory of destination file does not exist.")
    if (target.exists && !replaceExisting) fail("Destination file already exists, force creation or choose another file.")
    if (target.exists && !target.checkAccess(Write)) fail("Destination exists but is not writable.")
    if (target.isDirectory && target.children().nonEmpty) fail("Destination exists but is a non-empty directory.")

    if (isDirectory) target.createDirectory(createParents, false, access, attributes)
    else copyFile(target, createParents, copyAttributes, replaceExisting)
  }

  private def copyFile(dest: Path, createParents : Boolean, 
       copyAttributes: Boolean, replaceExisting: Boolean): Path = {
    val FIFTY_MB = 1024 * 1024 * 50
    assert(isFile, "Source %s is not a valid file." format name)

    if(createParents) dest.parent foreach {_ createDirectory(createParents=true, failIfExists=false)}
    
// TODO ARM this
    import scalax.io.OpenOption._
    for {inResource <- ops.fileChannel()
         in <- inResource
         out <- dest.ops.channel(Create, Truncate, Write)
    } {
      try {
        var pos, count = 0L
        while (pos < size) {
          count = (size - pos) min FIFTY_MB
          val prepos = pos
          pos += in.transferTo(pos, count, out)
          if(prepos == pos) fail("no data can be copied for unknown reason!")
        }
      }
      if (this.length != dest.length)
        fail("Failed to completely copy %s to %s".format(name, dest.name))

      if (copyAttributes)
        dest.lastModified = this.lastModified
    }
    dest
  }
  
  protected def moveFile(target: Path, atomicMove:Boolean) : Unit = {
    target match {
      case target : DefaultPath if jfile renameTo target.jfile => 
        () // moved worked as part of guard
      case _ =>
        target.ops writeInts this.ops.bytesAsInts
        delete()
    }
  }
  
  protected def moveDirectory(target:Path, depth:Int, atomicMove : Boolean) : Unit = {
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

  def descendants(filter:Path => Boolean, depth:Int, options:Traversable[LinkOption]) = new DefaultDirectoryStream(this, filter, depth) // TODO options (not supported until Java 7)

  def ops:FileOps = new DefaultFileOps(this, jfile)

}
