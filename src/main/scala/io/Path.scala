/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{ 
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, 
  BufferedInputStream, BufferedOutputStream, File => JFile }
import java.net.{ URI, URL }
import collection.{ Sequence, Traversable }
import collection.immutable.{ StringVector => SV }
import PartialFunction._
import util.Random.nextASCIIString
import java.lang.{ProcessBuilder, Process}

/**
 * The object for constructing Path objects and for containing implicits from strings and 
 * Java.io.Files to Scala files
 */
object Path
{
  // not certain these won't be problematic, but looks good so far
  implicit def string2path(s: String): Path = apply(s)
  implicit def jfile2path(jfile: JFile): Path = apply(jfile)
    
  // java 7 style, we don't use it yet
  // object AccessMode extends Enumeration("AccessMode") {
  //   val EXECUTE, READ, WRITE = Value
  // }
  // def checkAccess(modes: AccessMode*): Boolean = {
  //   modes foreach {
  //     case EXECUTE  => throw new Exception("Unsupported") // can't check in java 5
  //     case READ     => if (!jfile.canRead()) return false
  //     case WRITE    => if (!jfile.canWrite()) return false
  //   }
  //   true
  // }
  
  def roots: List[Path] = JFile.listRoots().toList map Path.apply

  def apply(path: String): Path = apply(new JFile(path))
  def apply(jfile: JFile): Path = new Path(jfile)
    
  private[io] def randomPrefix = nextASCIIString(6)
  private[io] def fail(msg: String) = throw new IOException(msg)
}
import Path._

/** 
 *  An abstraction for filesystem paths.  A path does not represent an
 *  underlying filesystem object rather it is a path to a potential filesystem
 *  object.  The path provides methods for inspecting if there exists
 *  referenced filesystem object and metadata about the object.  In 
 *  addition to path there are {@link File} and {@link Directory} "views". 
 *  <p>
 *  The differences between {@link Path}, {@link File}, and {@link Directory} are primarily to 
 *  communicate intent. Since the filesystem can change at any time, there 
 *  is no way to reliably associate Files only with files and so on.
 *  </p>
 *  <p>
 *  Any Path can be converted to a {@link File} or {@link Directory} (and thus gain access 
 *  to the additional entity specific methods) by calling {@link #toFile()} or
 *  {@link #toDirectory()}, which has no effect on the filesystem.
 *  </p>
 *  <p>
 *  It is important to remember that {@link File} is a view of a filesystem object
 *  it does not guarantee that the underlying object is a file and will not change
 *  For example, a File object can created and after an external process can change the 
 *  filesystem so the filesystem object is actually a Directory.
 *  </p>
 *  <p>
 *  Further it must be recognized that in some file systems a file could also be a directory.
 *  In which case both toFile and toDirectory will be valid.
 *  </p>
 *  <p>
 *  The Path constructor is private so we can enforce some
 *  semantics regarding how a Path might relate to the world.
 *  </p>
 *
 *  @author  Paul Phillips
 *  @author  Jesse Eichar
 *  @since   0.1
 * 
 *  @see File
 *  @see Directory
 */
class Path private[io] (val jfile: JFile)
{
  /** The path segment separator character */
  val separator = JFile.separatorChar

  /** Validation: this verifies that the type of this object and the
   * contents of the filesystem are in agreement.  All objects are
   * valid except File objects whose path points to a directory and
   * Directory objects whose path points to a file.
   */
  def isValid: Boolean = true

  // conversions
  /**
   * Convert the Path to a file object if the path is a File
   * or if it does not exist
   *
   * @see toDirectory
   */
  def toFile: Option[File] = if(isFile ¦¦ !exists) Some(new File(jfile)) else None
  /**
   * Convert the Path to a file object if the path is a File
   * or if it does not exist
   *
   * @see toFile
   */
  def toDirectory: Option[Directory] = if(isFile ¦¦ !exists) Some(new Directory(jfile)) else None
  /**
   * Modifies the Path so that it is absolute from a root of the file system.
   * However it is not necessarily canonical.  For example /home/user/../another 
   * is a valid absolute path.
   * @see toCanonical
   */
  def toAbsolute: Path = if (isAbsolute) this else Path(jfile.getAbsolutePath())
  /**
   * Creates a URI from the path.
   * @see java.io.File#toURI
   */
  def toURI: URI = jfile.toURI()
  /**
   * Creates a URL from the path.  This does have the bug present in {@link java.io.File#toURL}
   * and can be used directly.
   * @see java.io.File#toURI
   */
  def toURL: URL = toURI.toURL()
  
  /** 
   * If child is relative, creates a new Path based on the current path with the
   * child appended. If child is absolute the child is returned
   *
   * <p>Examples include:
   * <pre><code>
   * path / "child" / "grandchild"
   * path / "child/grandchild"
   * path / ".." / "sibling"
   * path / "../sibling"
   *
   * @return A new path with the specified path appended
   * @Note This is a duplicate when the implicit string2Path is imported
   *       But using the implicit makes the API less discoverable so I have 
   *       added this method.
   * @see #/(String)
   */  
  def /(child: String): Path = new Path(new JFile(jfile, child)) // TODO check if directory is absolute
  /** 
   * If child is relative, creates a new Path based on the current path with the
   * child appended. If child is absolute the child is returned
   *
   * <p>Examples include:
   * <pre><code>
   * path / Path("child") / Path("grandchild")
   * path / Path("child/grandchild")
   * path / Path("..") / Path("sibling")
   * path / Path("../sibling")
   * </code></pre></p>
   *
   * @return A new path with the specified path appended
   * @see #/(String)
   */
  def /(child: Path): Path = /(child.path)
  /**
   * If child is relative, creates a new Directory based on the current path with the
   * child appended. If child is absolute the child is returned
   *
   * @return A new Directory with the specified path appended
   * @see #/(String)
   */
  def /(child: Directory): Directory = /(child: Path).toDirectory
  /**
   * If child is relative, creates a new File based on the current path with the
   * child appended. If child is absolute the child is returned
   *
   * @return A new Directory with the specified path appended
   * @see #/(String)
   */
  def /(child: File): File = /(child: Path).toFile

  // identity
  /**
   * The name of the file.  This includes the extension of the file
   * @return the name of the file
   */
  def name: String = jfile.getName()
  /**
   * The path of the file.  It may or may not be relative
   *
   * @return the path of the file
   */
  def path: String = jfile.getPath()
  /**
   * Returns the related Path that starts at a root of the file system and is the direct 
   * path with all relative segments are resolved.
   * 
   * For example /home/user/../another is <em>not</em> a valid canonical path.
   * @see #toAbsolute
   * @see java.io.File#toCanonical   
   */
  def normalize: Path = Path(jfile.getCanonicalPath())
  /**
   * TODO Need to refer to NIO documentation to understand this
   */
  def resolve(other: Path): Path = null // TODO
  /**
   * Make the current path relative to the other path.  If the two paths
   * are on different drives then the other path is returned
   *
   * @return relative path from the current path to the other path
   * @NOTE do we want to relativize:  /home/jesse and /home/jones to /home/jesse/../jones?
           or do we call that out of scope and simply return other?
   */
  def relativize(other: Path): Path = null // TODO
  
  // derived from identity
  /**
   * The root of the file system of the path if it can be determined.
   *
   * @return the root of the file system
   * @NOTE do we want to convert to absolute to try to determine root or always return None
   *       if the path is relative?  (I think convert to absolute)
   */
  def root: Option[Path] = roots find (this startsWith _) // TODO convert to absolute?
  /**
   * The segments in the path including the current element of the path.  If the
   * the path is relative only the segments defined are returned... NOT the absolute 
   * path
   * @return the segments in the path
   */
  def segments: List[String] = (path split separator).toList filterNot (_.isEmpty)
  /**
   * The parent path segment if it exists
   * @return the parent path segment if it exists
   * @see parents
   */
  def parent: Option[Path] = Option(jfile.getParent()) map Path.apply
  /**
   * The path segments of the path excluding the current path segment
   * @return The path segments of the path excluding the current path segment
   * @see segments
   */
  def parents: List[Path] = parent match {
    case None     => Nil
    case Some(p)  => p :: p.parents
  }
  /**
   * The extension of the name of the path, if it exists. if name ends with an 
   * extension (e.g. "foo.jpg") returns the extension ("jpg")
   *
   * @return the extension of the path
   */
  def extension: Option[String] =   
    condOpt(SV.lastIndexWhere(name, _ == '.')) {
      case idx if idx != -1 => SV.drop(name, idx + 1)
    }
  // Alternative approach:
  // (Option fromReturnValue SV.lastIndexWhere(name, _ == '.') map (x => SV.drop(name, x + 1))

  // Boolean tests
  /**
   * True if the backing element exists and is readable
   * @return True if the backing element exists and is readable
   * @see java.io.File#canRead
   */
  def canRead = jfile.canRead()
  /**
   * True if the backing element exists and is writable
   * @return True if the backing element exists and is writable
   * @see java.io.File#canWrite
   */
  def canWrite = jfile.canWrite()
  /**
   * True if the backing element exists and is Executable
   * @return True if the backing element exists and is Executable
   * @see java.io.File#canExecute
   */
  def canExecute = jfile.canExecute()
  /**
   * True if the path exists in the file system
   *
   * @return true if the path exists in the file system
   * @see java.io.File#exists
   */
  def exists = jfile.exists()
  /**
   *  False if the path does not exist in the file system
   *
   * @return false if the path does not exist in the file system
   */
  def notExists = try !jfile.exists() catch { case ex: SecurityException => false }
  /**
   * True if the path exists and is a file
   * 
   * @return true if the path exists and is a file
   * @see java.io.File#isFile
   */
  def isFile = jfile.isFile()
  /**
   * True if the path exists and is a directory
   * 
   * @return true if the path exists and is a directory
   * @see java.io.File#isDirectory
   */
  def isDirectory = jfile.isDirectory()
  /**
   * True is the file is absolute.  
   * IE is rooted at a filesystem root
   * @return true if file is absolute.
   * @see java.io.File#isAbsolute
   */
  def isAbsolute = jfile.isAbsolute()
  /**
   * True if the file is a hidden file for the current
   * filesystem
   * 
   * @return True if the file is a hidden file for the current
   * filesystem
   * @see java.io.File#isHidden()
   */
  def isHidden = jfile.isHidden()
  /**
   * True if the file is a symlink.
   * <p>This method is generally correct but depending
   * the filesystem there is a possibility of getting the
   * the incorrect result since the canonical and absolute
   * paths are compared and no native code is used.  Future
   * versions will be guaranteed to work correctly but this version
   * cannot be because of limitations of the VM.</p>
   *
   * @return True if the file is a symlink.
   */
  def isSymlink = parent.isDefined && {    
    val x = parent.get / name
    x.normalize != x.toAbsolute
  }
  
  // Information
  /**
   * The time of the last modification of the file
   *
   * @return True if the file is a symlink.
   * @see java.io.File#getLastModified()
   */
  def lastModified = jfile.lastModified()
  /**
   * Set the last time modified of the file
   *
   * @return the new time
   * @see java.io.File#setLastModified(Long)
   */
  def lastModified_=(time: Long) = {jfile setLastModified time; time}
  /**
   * The length of the file in bytes or 0 if file does not exist or is not a file
   *
   * @return The length of the file in bytes or 0 if file does not exist or is not a file
   * @see java.io.File#length()
   */
  def length = jfile.length()
  
  // Boolean path comparisons
  /**
   * True if this path ends with the other path
   * @return True if this path ends with the other path
   */
  def endsWith(other: Path):Boolean = segments endsWith other.segments
  /**
   * True if this path starts with the other path
   * @return True if this path starts with the other path
   */
  def startsWith(other: Path):Boolean = segments startsWith other.segments
  /**
   * True if this path and the other path reference the same file.  
   * <p>
   * This means the two paths may have different segments and perhaps
   * even have symlinks in the path but if they reference the same file
   * then this will return true.  Use equals to test if the paths are the same
   * </p>
   * 
   * @return True if this path and the other path reference the same file.  
   */
  def isSame(other: Path):Boolean = normalize == other.normalize
  /**
   * True if this path has been modified more recently than other.
   * If this file does not exist it is not fresh than other
   * 
   * @return True if this path has been modified more recently than other.
   */
  def isFresher(other: Path):Boolean = lastModified > other.lastModified
  /**
   * TODO
   */
  def compareTo(other:Path):Int = 0 // TODO

  // creations
  /**
   * Create the file referenced by this path.  
   * <p>
   * If failIfExists then IOException is thrown if the file already exists. 
   * In the next Java 7 only version it will throw FileAlreadyExistsException
   * </p>
   * @throws IOException if file already exists.  In the next Java 7 
   *         only version it will throw FileAlreadyExistsException
   */
  def createFile(failIfExists: Boolean = false /*, attributes:List[FileAttributes[_]]=Nil TODO*/): File = {
    val res = jfile.createNewFile()
    if (!res && failIfExists && exists) fail("File '%s' already exists." format name)
    else if (isFile) toFile
    else new File(jfile)
  }
  /**
   * Create the directory referenced by this path.  
   * <p>
   * If failIfExists then FileAlreadyExistsException is thrown if the directory already exists
   * In the next Java 7 only version it will throw FileAlreadyExistsException
   * </p>
   * @throws IOException if directory already exists.  In the next Java 7 only version it will 
   *         throw FileAlreadyExistsException
   *
   */
  def createDirectory(force: Boolean = true, failIfExists: Boolean = false /*, attributes:List[FileAttributes[_]]=Nil TODO*/): Directory = {
    val res = if (force) jfile.mkdirs() else jfile.mkdir()
    if (!res && failIfExists && exists) FileAlreadyExistsExcepion("Directory '%s' already exists." format name)
    else if (isDirectory) toDirectory
    else new Directory(jfile)
  }

 
  // deletions
  /**
   *  Delete the filesystem object if possible.  
   *  <p>
   *  If the file exists and is a non-empty Directory or 
   *  there is some other reason the operation cannot be performed an 
   *  IOException will be thrown.
   *  </p>
   *  <p>
   *  If the file does not exist it will return false
   *  </p>
   */
  def delete() = if (jfile.exists()) delete() else false

  // todo
  /**
   *  Copy the underlying object if it exists to the target location.  
   *  If the underlying object is a directory it is not copied recursively.
   *
   *  @param target 
   *      the target path to copy the filesystem object to.
   *  @param copyAttributes 
   *      if true then copy the File attributes of the object
   *      as well as the data.  True by default
   *  @param replaceExisting 
   *      if true then replace any existing target object
   *      unless it is a non-empty directory in which case
   *      an IOException is thrown.
   *      False by default
   * 
   *  @return 
   *      true if data was copied false if this path does not reference an object
   *  @throws IOException 
   *      if the copy could not be satisfied because the target could
   *      not be written to or if this path cannot be copied
   */
  def copyTo(target: Path, copyAttributes:Boolean=true, 
             replaceExisting=false, atomicMove:Boolean=false): Boolean = false
  /**
   *  Move the underlying object if it exists to the target location.  
   *
   *  @param target          
   *      the target path to move the filesystem object to.
   *  @param replaceExisting 
   *      if true then replace any existing target object
   *      unless it is a non-empty directory in which case
   *      an IOException is thrown.
   *      False by default
   *  @param atomicMove      
   *      This is ignored at the moment but in the future version
   *      it will guarantee atomicity of the move
   *      True by default
   *
   *  @return true
   *      if data was moved false if this path does not reference an object
   *  @throws IOException 
   *      if the move could not be satisfied because the target could
   *      not be written to or if this path cannot be moved
   */
  def moveTo(target: Path, options:CopyOption*): Boolean = false
  
  override def toString() = "Path(%s)".format(path)
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }  
  override def hashCode() = path.hashCode()

  /**
   * Execute the file in a separate process if the path
   * is executable.
   *
   * @param arguments to send to the process
   * @return Process
   */
  def execute(args:Seq[String])(configuration:ProcessBuilder=>Unit):Process = null // TODO
  
}
