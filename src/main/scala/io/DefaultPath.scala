/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.resource.ManagedResource
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
 *  A file reference that locates a file using a system independent path.
 *  The file is not required to exist.
 *
 *  @author  Paul Phillips
 *  @author  Jesse Eichar
 *  @since   0.1
 * 
 */
class DefaultPath private[io] (val jfile: JFile, fileSystem: FileSystem) extends Path(fileSystem)
{
  def toAbsolute: Path = if (isAbsolute) this else Path(jfile.getAbsolutePath())(fileSystem)
  def toURI: URI = jfile.toURI()
  def /(child: String): Path = Path(new JFile(jfile, child)) // TODO check if directory is absolute
  def name: String = jfile.getName()
  def path: String = jfile.getPath()
  def normalize: Path = fileSystem(jfile.getCanonicalPath())
  def parent: Option[Path] = Option(jfile.getParent()) map Path.apply
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
  def length = jfile.length()
  private def createContainingDir(createParents: Boolean) = {
    def testWrite(parent:Option[Path]) = {
      parent match {
        case Some(p) if(!p.canWrite) => fail("Cannot write parent directory")
        case Some(p) => ()
        case None => fail("Parent directory cannot be created")
      }
    }

    (createParents, parent) match {
      case (_, Some(_)) => testWrite(parent)
      case (true, None) => {
        jfile.getParentFile.mkdirs()
        testWrite(parent)
      }
      case (false, None) => fail("Parent directory does not exist")
    }
  }
  def createFile(createParents: Boolean = true, failIfExists: Boolean = true /*, attributes:List[FileAttributes[_]]=Nil TODO*/): Path = {
    createContainingDir (createParents)
    val res = jfile.createNewFile()
    if (!res && failIfExists && exists) fail("File '%s' already exists." format name)
    else this
  }
  def createDirectory(createParents: Boolean = true, failIfExists: Boolean = true /*, attributes:List[FileAttributes[_]]=Nil TODO*/): Path = {
    createContainingDir (createParents)

    val res = jfile.mkdir()

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
  def execute(args:String*)(implicit configuration:ProcessBuilder=>Unit = p =>()):Option[Process] = null // TODO
  
  override def toString() = "Path(%s)".format(path)
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }  
  override def hashCode() = path.hashCode()

  //Directory accessors
/*  def directoryStream (function: PartialFunction[Path,Unit]): Unit = { null
    foldDirectoryStream (()){case (_,path) => function(path)}
  }
  def foldDirectoryStream[R] (initial:R)(function: PartialFunction[(R, Path),R]): Option[R] = {
    val (foundMatch, result) = jfile.listFiles
         .map (f=> fileSystem(f.getPath))
         .foldLeft ((false, initial)) {
           case ((_, result), nextPath) if(function.isDefinedAt(result,nextPath)) => (true, function(result, nextPath))
           case ((foundMatch, result), _ ) => (foundMatch, result)
         }
    if(foundMatch) Some(result)
    else None
  }
*/
  def tree(filter:(Path,Path)=>Option[PathMatcher] = (origin,relativePath) => None, 
           depth:Int = -1, 
           lock: Boolean = false) : DirectoryStream[Path] = {
    null // TODO
  }

  def directoryStream(filter:Option[PathMatcher] = None, 
                      lock: Boolean = false) : DirectoryStream[Path] =tree((o,p) => filter, 1, lock)

}
