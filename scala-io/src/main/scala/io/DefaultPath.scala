/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.resource.ManagedResource
import scalax.io.attributes.FileAttribute

import java.io.{ 
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, 
  BufferedInputStream, BufferedOutputStream, IOException, File => JFile}
import java.net.{ URI, URL }
import collection.{Traversable }
import PartialFunction._
import util.Random.nextASCIIString
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
private[io] class DefaultPath private[io] (val jfile: JFile, fileSystem: DefaultFileSystem) extends Path(fileSystem)
{
  def toAbsolute: Path = if (isAbsolute) this else Path(jfile.getAbsolutePath())(fileSystem)
  def toURI: URI = jfile.toURI()
  def /(child: String): Path = Path(new JFile(jfile, child)) // TODO check if directory is absolute
  def name: String = jfile.getName()
  def path: String = jfile.getPath()
  def normalize: DefaultPath = fileSystem(jfile.getCanonicalPath())
  def parent: Option[DefaultPath] = Option(jfile.getParent()) map fileSystem.apply
  def checkAccess(modes: AccessMode*): Boolean = {
    modes foreach {
      case EXECUTE  => if (!jfile.canExecute()) return false
      case READ     => if (!jfile.canRead())    return false
      case WRITE    => if (!jfile.canWrite())   return false
    }
    true
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

    jfile.setReadable(accessModes exists {_==READ})
    jfile.setWritable(accessModes exists {_==WRITE})
    jfile.setExecutable(accessModes exists {_==EXECUTE})
    
  }
  def access : Set[AccessMode] = {
    AccessModes.values filter { 
      case READ => canRead
      case WRITE => canWrite
      case EXECUTE => canExecute
    }
  }
  
  
  private def createContainingDir(createParents: Boolean) = {
    def testWrite(parent:Option[Path]) = {
      parent match {
        case Some(p) if(!p.canWrite) => fail("Cannot write parent directory: "+p+" of "+ path)
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
      case (false, None) => fail("Parent directory does not exist")
    }
  }
  def createFile(createParents: Boolean = true, failIfExists: Boolean = true, 
                 accessModes:Iterable[AccessMode]=List(READ,WRITE), attributes:Iterable[FileAttribute[_]]=Nil): Path = {
    createContainingDir (createParents)
    val res = jfile.createNewFile()
    access = accessModes
    if (!res && failIfExists && exists) fail("File '%s' already exists." format name)
    else this
  }
  def createDirectory(createParents: Boolean = true, failIfExists: Boolean = true,  
                      accessModes:Iterable[AccessMode]=List(READ,WRITE),attributes:Iterable[FileAttribute[_]]=Nil) = {

    createContainingDir (createParents)

    val res = jfile.mkdir()
    access = accessModes
    if (!res && failIfExists && exists) fail("Directory '%s' already exists." format name)
    else this
  }
  def delete(): Unit = {
    if (!(canWrite && jfile.delete)) {
      fail("File is not writeable so the file cannot be deleted")
    }
    ()
  }
  def deleteRecursively(continueOnFailure:Boolean=false): (Int,Int) = deleteRecursively(jfile,continueOnFailure)
  private def deleteRecursively(f: JFile, continueOnFailure:Boolean): (Int,Int) = {
    def combine(one:(Int,Int),two:(Int,Int)) = (one._1 + two._1, one._2 + two._2)
    val (deleted:Int,remaining:Int) = if (f.isDirectory) f.listFiles match { 
      case null => (0,0)
      case xs   => (xs foldLeft (0,0)){case (count,path) => combine (count, path deleteRecursively continueOnFailure) }
    }
    (f.delete(),continueOnFailure) match {
      case (true, _) => (deleted + 1, remaining)
      case (false, true) => (deleted, remaining + 1)
      case (false, false) => fail( "Unable to delete "+f);
    }
  }
  def copyTo(target: Path, copyAttributes:Boolean=true, 
             replaceExisting:Boolean=false): Path = null //TODO

  private def copyFile(dest: Path, copyAttributes: Boolean=true, 
             replaceExisting: Boolean=false): Path = {
    val FIFTY_MB = 1024 * 1024 * 50
    if (!isFile) fail("Source %s is not a valid file." format name)
    if (this.normalize == dest.normalize) fail("Source and destination are the same.")
    if (!dest.parent.map(_.exists).getOrElse(false)) fail("Parent directory of destination file does not exist.")
    if (dest.exists && !replaceExisting) fail("Destination file already exists, force creation or choose another file.")
    if (dest.exists && dest.checkAccess(WRITE)) fail("Destination exists but is not writable.")
    if (dest.isDirectory) fail("Destination exists but is a directory.")

// TODO ARM this
    lazy val in_s = new FileInputStream(jfile)
    lazy val out_s = new FileOutputStream(dest.asInstanceOf[DefaultPath].jfile)
    lazy val in = in_s.getChannel()
    lazy val out = out_s.getChannel()

    try {
      val size = in.size()
      var pos, count = 0L
      while (pos < size) {
        count = (size - pos) min FIFTY_MB
        pos += out.transferFrom(in, pos, count)
      }
    }
    finally List[Closeable](out, out_s, in, in_s) foreach closeQuietly
    
    if (this.length != dest.length)
      fail("Failed to completely copy %s to %s".format(name, dest.name))
    
    if (copyAttributes)
      dest.lastModified = this.lastModified
    
    dest
  }
  def moveTo(target: Path, replaceExisting:Boolean=false, 
             atomicMove:Boolean=false): Path = null

  override def toString() = "Path(%s)".format(path)
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }  
  override def hashCode() = path.hashCode()

  def directoryStream(filter:Option[PathMatcher] = None) = null // TODO

  def tree(filter:(Path,Path)=>Option[PathMatcher] = (origin,relativePath) => None, 
           depth:Int = -1) = null // TODO

  def fileOps:FileOps = new DefaultFileOps(this, jfile)

}
