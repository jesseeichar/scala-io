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
  BufferedInputStream, BufferedOutputStream, IOException, File => JFile}
import java.net.{ URI, URL }
import collection.{ Sequence, Traversable }
import collection.immutable.StringVector
import PartialFunction._
import util.Random.nextASCIIString
import java.lang.{ProcessBuilder, Process}

/**
 * The object for constructing Path objects and for containing implicits from strings and 
 * {@link java.io.File}s to Scala paths. 
 * <p> All Paths constructed by this factory are created for the default filesystem</p>
 */
object Path
{
  /** 
   * Method to implicitly convert a string to a Path
   * object on the default file system
   */
  implicit def string2path(s: String): Path = apply(s)
  /** 
   * Method to implicitly convert a {@link java.io.File} to a Path
   * object on the default file system
   */
  implicit def jfile2path(jfile: JFile): Path = apply(jfile)
    
  /**
   * Enumeration of the Access modes possible for accessing files
   */
  object AccessModes extends Enumeration("AccessMode") {
    type AccessMode = Value
    val EXECUTE, READ, WRITE = Value
  }

  /**
   * Lists the roots of the default filesystem
   */  
  def roots: List[Path] = JFile.listRoots().toList map Path.apply

  /**
   * Create a Path on the default files system from a string
   *
   * @param path 
   *          the string to use for creating the Path
   */
  def apply(path: String): Path = apply(new JFile(path))
  /**
   * Create a Path on the default files system from a {@link java.io.File}
   *
   * @param path 
   *          the file to use for creating the Path
   */
  def apply(jfile: JFile): Path = new Path(jfile)

  /**
   * Creates an empty file in the provided directory with the provided prefix and suffixes.
   * The file will not replace an existing file and it is guaranteed to be unique and
   * not previously used by another process at time of creation.
   * 
   * @param prefix 
   *          the starting characters of the file name.
   *          Default is a randomly generated prefix
   * @param suffix
   *          the last characters of the file name
   *          Default is null (no suffix)
   * @param dir
   *          the directory to create the file in.  If null or
   *          not declared the file will be created in the system
   *          temporary folder
   *          Default is null (system/user temp folder)
   * @param deleteOnExit
   *          If true then the file will be deleted when the JVM is shutdown
   *          Default is true
   * @param attributes
   *          The attributes to create on the file.
   *          Default is Nil(default system file attributes)
   */
  def makeTempFile(prefix: String = Path.randomPrefix, 
                   suffix: String = null, 
                   dir: Path = null,
                   deleteOnExit : Boolean = true
                   /*attributes:List[FileAttributes] TODO */ ) : Path = {
    val file = apply(JFile.createTempFile(prefix, suffix, dir.jfile))
    if(deleteOnExit) file.jfile.deleteOnExit
    file
  }
    

  /**
   * Creates an empty directory in the provided directory with the provided prefix and suffixes.
   * The directory will not replace an existing file/directory and it is guaranteed to be unique and
   * not previously used by another process at time of creation.
   * 
   * @param prefix 
   *          the starting characters of the directory name.
   *          Default is a randomly generated prefix
   * @param suffix
   *          the last characters of the directory name
   *          Default is null (no suffix)
   * @param dir
   *          the directory to create the directory in.  If null or
   *          not declared the directory will be created in the system
   *          temporary folder
   *          Default is null (system/user temp folder)
   * @param deleteOnExit
   *          If true then the directory and all contained folders will be deleted
   *          when the JVM is shutdown. 
   *          Default is true
   * @param attributes
   *          The attributes to create on the file.
   *          Default is Nil(default system file attributes)
   */
  def makeTempDirectory(prefix: String = Path.randomPrefix,
                        suffix: String = null, 
                        dir: Path = null,
                        deleteOnExit : Boolean = true
                        /*attributes:List[FileAttributes] TODO */) : Path = {
    val path = Path.makeTempFile(prefix, suffix, dir.jfile, false)
    path.delete()
    path.createDirectory()
    if(deleteOnExit) {
      Runtime.getRuntime.addShutdownHook(new Thread{ def run:Unit = path.deleteRecursively(true) })
    }
    path
  }

  type Closeable = { def close(): Unit }
  private[io] def closeQuietly(target: Closeable) {
    try target.close() catch { case e: IOException => }
  }  

  private[io] def randomPrefix = nextASCIIString(6)
  private[io] def fail(msg: String) = throw new IOException(msg)
}
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
class Path private[io] (val jfile: JFile) extends Ordered[Path]
{
  /** The path segment separator string */
  val separator = JFile.separator

  // conversions
  /**
   * Modifies the Path so that it is absolute from a root of the file system.
   * However it is not necessarily canonical.  For example /home/user/../another 
   * is a valid absolute path.
   *
   * @see normalize
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
   * </code></pre>
   * <p>
   * @Note This is a duplicate when the implicit string2Path is imported
   *       But using the implicit makes the API less discoverable so I have 
   *       added this method.
   * </p>
   * @return A new path with the specified path appended
   *
   * @see Path#/(String)
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
   * <p>
   * @NOTE do we want to relativize:  /home/jesse and /home/jones to /home/jesse/../jones?
   *       or do we call that out of scope and simply return other?
   * </p>
   * @return relative path from the current path to the other path
   */
  def relativize(other: Path): Path = null // TODO
  
  // derived from identity
  /**
   * The root of the file system of the path if it can be determined.
   * <p>
   * @NOTE do we want to convert to absolute to try to determine root or always return None
   *       if the path is relative?  (I think convert to absolute)
   * </p>
   *
   * @return the root of the file system
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
    condOpt(StringVector.lastIndexWhere(name, _ == '.')) {
      case idx if idx != -1 => StringVector.drop(name, idx + 1)
    }
  // Alternative approach:
  // (Option fromReturnValue StringVector.lastIndexWhere(name, _ == '.') map (x => StringVector.drop(name, x + 1))

  // Boolean tests
  /**
   * Check if the referenced file both exists and be accessed with the requested modes
   * 
   * @param modes the modes to check for on the file.  If empty then only existance 
   *        is checked
   * @return true if all modes are available on the file
   */
  def checkAccess(modes: AccessMode*): Boolean = {
    modes foreach {
      case EXECUTE  => if (!jfile.canExecute()) return false
      case READ     => if (!jfile.canRead())    return false
      case WRITE    => if (!jfile.canWrite())   return false
    }
    true
  }
  def canWrite : Boolean = jfile.canWrite
  def canRead : Boolean = jfile.canRead
  def canExecute : Boolean = jfile.canExecute
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
   * Compares this path to the other lexigraphically.  
   */
  def compareTo(other:Path):Int = toString.compareTo(other.toString)

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
  def createFile(failIfExists: Boolean = false /*, attributes:List[FileAttributes[_]]=Nil TODO*/): Path = {
    val res = jfile.createNewFile()
    if (!res && failIfExists && exists) fail("File '%s' already exists." format name)
    else this
  }
  /**
   * Create the directory referenced by this path.  
   * <p>
   * If failIfExists then FileAlreadyExistsException is thrown if the directory already exists
   * In the next Java 7 only version it will throw FileAlreadyExistsException
   * </p>
   * @throws IOException if directory already exists.  In the next Java 7 only version it will 
   *         throw IOException
   *
   */
  def createDirectory(force: Boolean = true, failIfExists: Boolean = false /*, attributes:List[FileAttributes[_]]=Nil TODO*/): Path = {
    val res = if (force) jfile.mkdirs() else jfile.mkdir()
    if (!res && failIfExists && exists) fail("Directory '%s' already exists." format name)
    else this
  }
 
  // deletions
  /**
   *  Delete the filesystem object if the file exists.
   *  <p>
   *  If the file exists and is a non-empty Directory or 
   *  there is some other reason the operation cannot be performed an 
   *  IOException will be thrown.
   *  </p>
   *  <p>
   *  If the file does not exist it will return false
   *  </p>
   *  @throws IOException if the file cannot be written
   */
  def deleteIfExists() = {
    if (jfile.exists()) {
	delete()
    } else {
      false
    }
  }
  
  def delete() = if( canWrite ) jfile.delete 
                 else fail("File is not writeable so the file cannot be deleted")

  /** 
   *  Deletes the directory recursively.
   * <p> 
   *  This method does not detect circular directory graphs and
   *  does not promise to perform the delete in an atomic operation
   * </p>
   *  <p>Use with caution!</p>
   *  @param continueOnFailure 
   *           If false then method will abort when encountering a
   *           file that cannot be deleted.  Otherwise it will continue
   *           to delete all the files that can be deleted. 
   *           Note:  this method is not transactional, all files visited before
   *           failure are deleted.  
   *  
   *  @return 
   *           Tuple with (The number of files deleted, The number of files remaining)
   *  @throws IOException 
   *           when continueOnFailure is false and a file cannot be deleted
   */
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
   *      the path to the new copy
   *  @throws IOException 
   *      if the copy could not be satisfied because the target could
   *      not be written to or if this path cannot be copied
   */
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
    lazy val out_s = new FileOutputStream(dest.jfile)
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
   *      the path to the moved object
   *  @throws IOException 
   *      if the move could not be satisfied because the target could
   *      not be written to or if this path cannot be moved
   */
  def moveTo(target: Path, replaceExisting:Boolean=false, 
             atomicMove:Boolean=false): Path = null
  
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

  //Directory accessors

}
