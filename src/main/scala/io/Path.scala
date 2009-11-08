/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.resource.ManagedResource
import scalax.io.attributes.FileAttribute
import java.io.{IOException, File => JFile}
import java.nio.channels.ByteChannel
import java.net.{ URI, URL }
import collection.{ Traversable }
import PartialFunction._
import util.Random.nextASCIIString
import java.lang.{ProcessBuilder}
import FileSystem.defaultFileSystem
/**
 * The object for constructing Path objects and for containing implicits from strings and
 * {@link java.io.File}s to Scala paths.
 * <p> All Paths constructed by this factory are created for the default filesystem</p>
 * 
 *  @author Jesse Eichar
 *  @since  0.1
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
  implicit def jfile2path(jfile: JFile): Path = apply(jfile.getPath)

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
  def roots: List[Path] = defaultFileSystem.roots

  /**
   * Create a Path from a string
   *
   * @param path
   *          the string to use for creating the Path
   * @param filesystem
   *          the filesystem that the path is valid for/will be
   *          used for creating the Path object
   *          Default is the default filesystem
   *          This is an implicit parameter
   */
  def apply(path: String)(implicit fileSystem: FileSystem = defaultFileSystem): Path = fileSystem(path)

  /**
   * Create a Path from a URI.
   * <p>
   * both the filesystem and the path can be determined from
   * from the uri.  For example:
   * <ul>
   * <li><code>file://c:\dir</code> will create an absolute Path
   *     from the default filesystem with the Path = c:\dir</li>
   * <li><code>jar://classes.jar!dir</code> creates a Path to the
   *     dir directory in the jar filesystem classes.jar</li>
   * </ul>
   * @param uri
   *          The uri specifying the filesystem and path
   * @return
   *          A path object
   * @throws IOException
   *          if the filesystem cannot be loaded or if the
   *          path cannot be created with that filesystem
   */
  def apply (uri: URI): Path = null // TODO

  /**
   * Create a Path on the default files system from a {@link java.io.File}
   *
   * @param path
   *          the file to use for creating the Path
   */
  def apply(jfile: JFile) = defaultFileSystem(jfile.getPath)

  /**
   * Creates an empty file in the provided directory with the provided prefix and suffixes.
   * The file will not replace an existing file and it is guaranteed to be unique and
   * not previously used by another process at time of creation.
   *
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
   * @param fileSystem
   *          the filesystem that will create the temporary object
   *          Default is the default filesystem
   */
  def makeTempFile(prefix: String = Path.randomPrefix,
                   suffix: String = null,
                   dir: String = null,
                   deleteOnExit : Boolean = true,
                   attributes:Iterable[FileAttribute[_]] = Nil )
                  (implicit fileSystem: FileSystem = defaultFileSystem) : Path = {
    fileSystem.makeTempDirectory(prefix,suffix,dir,deleteOnExit)
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
                        dir: String = null,
                        deleteOnExit : Boolean = true,
                        attributes:Iterable[FileAttribute[_]] = Nil)
                       (implicit fileSystem: FileSystem = defaultFileSystem) : Path = {
    defaultFileSystem.makeTempDirectory(prefix,suffix,dir,deleteOnExit)
  }

  /**
   * Allows matching on the full path of a Path
   * <p>
   *  The match must be exact and in the same case as
   *  the path but the / and \ are interchangeable
   * </p>
   * <pre><code>
   * Path("c:/dir/file")
   * Path("c:\\dir\\file")
   * </code></pre>
   * both of the above examples will match the same path
   * <p>
   * For more on matching Paths see {@link Path.Matching}
   */
  def unapply(pathExpr:Path): Option[String] = None //TODO

  /**
   * Object containing several objects that make matching
   * certain types of Paths much easier.
   * <p>
   * Example:
   * <pre><code>
   * val Apple = path.matcher (pattern="[aA]pple", syntax="glob")
   * val ReadWrite = new AccessMatcher (READ, WRITE)
   * path match {
   *   case File(f) => println("A file was found")
   *   case Directory(d) => println("A directory was found")
   *   case NonExistant(e) => println("Path does not Exist")
   *   case Apple(apple) => println("A path named apple was found")
   *   case ReadWrite(apply) => println("Path is readable and writeable")
   *   case Path("c:/dir/file") => println("Found a path that is c:/dir/file or
                                            c:\\dir\\file")
   * }
   * </code></pre>
   * Note: The Apple portion of the example is not part of Matching but
   *       a critical part of matching Paths and thus is included in
   *       this example
   */
  object Matching {
    /** matches a path if it is a file (and exists)*/
    object File {
      def unapply(path:Path):Option[Path] = None // TODO
    }
    /** matches a path if it is a Directory (and exists)*/
    object Directory {
      def unapply(path:Path):Option[Path] = None // TODO
    }
    /** matches a path if it is Exists */
    object Exists {
      def unapply(path:Path):Option[Path] = None // TODO
    }
    /** matches a path if it does not Exist */
    object NonExistant {
      def unapply(path:Path):Option[Path] = None // TODO
    }
    /**
     * Matches a path if the access modes are applicable
     * for the file.
     * <p>
     * If the file does not exist this matcher will not match
     * </p>
     *
     * @param accessModes
     *          the access modes that must be applicable
     *          on the path object.
     */
    class AccessMatcher (accessModes: AccessModes.AccessMode*) {
	def unapply(path: Path): Option[Path] = None // TODO
    }
  }
  type Closeable = { def close(): Unit }
  private[io] def closeQuietly(target: Closeable) {
    try target.close() catch { case e: IOException => }
  }

  private[io] def randomPrefix = nextASCIIString(6)
  private[io] def fail(msg: String) = throw new IOException(msg)
}

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
abstract class Path (val fileSystem: FileSystem) extends Ordered[Path]
{
  /**
   * The path segment separator string for
   * the filesystem
   *
   * @see FileSystem#separator
   */
  val separator:String = fileSystem.separator

  // conversions
  /**
   * Modifies the Path so that it is absolute from a root of the file system.
   * However it is not necessarily canonical.  For example /home/user/../another
   * is a valid absolute path.
   *
   * @see normalize
   */
  def toAbsolute: Path
  /**
   * Creates a URI from the path.
   * @see java.io.File#toURI
   */
  def toURI: URI
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
   * <ul>
   * <li>if other is null return this</li>
   * <li>if other is absolute return other</li>
   * <li>if other is not absolute the return this append other</li>
   * </ul>
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
  def /(child: String): Path

  /**
   * If child is relative, creates a new Path based on the current path with the
   * child appended. If child is absolute the child is returned
   *
   * <ul>
   * <li>if other is null return this</li>
   * <li>if other is absolute return other</li>
   * <li>if other is not absolute the return this append other</li>
   * </ul>
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
  def name: String
  /**
   * The path of the file.  It may or may not be relative
   *
   * @return the path of the file
   */
  def path: String
  /**
   * Returns the related Path that starts at a root of the file system and is the direct
   * path with all relative segments are resolved.
   *
   * For example /home/user/../another is <em>not</em> a valid canonical path.
   * @see #toAbsolute
   * @see java.io.File#toCanonical
   */
  def normalize: Path
  /**
   * Resolve this path with other.  In the simplest case
   * that means appending other to this.
   * <p> Does the same thing as the / method </p>
   *
   * @param other
   *          another path to append to this path
   * @return
   *          the constructed/resolved path
   *
   */
  def resolve(other: Path): Path = null // TODO
  /**
   * Constructs a path from other using the same file system as this
   * path and resolves the this and other in the same manner as
   * {@link Path#resolve(Path)}
   *
   */
  def resolve(other: String): Path = resolve(fileSystem(other))

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
  def root: Option[Path] = fileSystem.roots find (this startsWith _) // TODO convert to absolute?
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
  def parent: Option[Path]
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
    name.lastIndexWhere (_ == '.') match {
      case idx if idx != -1 => Some(name.drop(idx + 1))
      case _ =>None
    }

  // Boolean tests
  /**
   * Check if the referenced file both exists and be accessed with the requested modes
   *
   * @param modes the modes to check for on the file.  If empty then only existance
   *        is checked
   * @return true if all modes are available on the file
   */
  def checkAccess(modes: AccessMode*): Boolean
  def canWrite : Boolean
  def canRead : Boolean
  def canExecute : Boolean
  /**
   * True if the path exists in the file system
   *
   * @return true if the path exists in the file system
   * @see java.io.File#exists
   */
  def exists: Boolean
  /**
   *  False if the path does not exist in the file system
   *
   * @return false if the path does not exist in the file system
   */
  def notExists = try !exists catch { case ex: SecurityException => false }
  /**
   * True if the path exists and is a file
   *
   * @return true if the path exists and is a file
   * @see java.io.File#isFile
   */
  def isFile: Boolean
  /**
   * True if the path exists and is a directory
   *
   * @return true if the path exists and is a directory
   * @see java.io.File#isDirectory
   */
  def isDirectory: Boolean
  /**
   * True is the file is absolute.
   * IE is rooted at a filesystem root
   * @return true if file is absolute.
   * @see java.io.File#isAbsolute
   */
  def isAbsolute: Boolean
  /**
   * True if the file is a hidden file for the current
   * filesystem
   *
   * @return True if the file is a hidden file for the current
   * filesystem
   * @see java.io.File#isHidden()
   */
  def isHidden: Boolean
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
  def lastModified:Long
  /**
   * Set the last time modified of the file
   *
   * @return the new time
   * @see java.io.File#setLastModified(Long)
   */
  def lastModified_=(time: Long): Long
  /**
   * The length of the file/directory in bytes or 0 if file does not exist
   *
   * @return The length of the file/directory in bytes or 0 if file does not exist
   * @see java.io.File#length()
   */
  def length: Long

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
  def isSame(other: Path): Boolean = normalize == other.normalize
  /**
   * True if this path has been modified more recently than other.
   * If this file does not exist it is not fresh than other
   *
   * @return True if this path has been modified more recently than other.
   */
  def isFresher(other: Path): Boolean = lastModified > other.lastModified
  /**
   * Compares this path to the other lexigraphically.
   */
  def compare(other:Path):Int = toString.compare(other.toString)

  // creations
  /**
   * Create the file referenced by this path.
   *
   * <p>
   * If failIfExists then IOException is thrown if the file already exists.
   * In the next Java 7 only version it will throw FileAlreadyExistsException
   * </p>
   * <p>
   * An exception is always thrown if the file is a directory and that directory
   * contains children
   * </p>
   * <p>
   * An Wxception will also be thrown if the parent directory does not have write
   * permission
   * </p>
   * @param createParents
   *          If true then the containing directories will be created if they do not exist
   *          Default is true
   * @param failIfExists
   *          If true and an object exists then an exception will be thrown
   *          If false then the object will be deleted if possible
   *          If not possible to delete the object or it is a non-empty directory
   *            an exception will be thrown
   *          Default is true
   *
   * @throws IOException
   *           If file or directory already exists.
   *           In the next Java 7 only version it will throw FileAlreadyExistsException
   *           If the process does not have write permission to the parent directory
   *           If parent directory does not exist
   */
  def createFile(createParents:Boolean = true, failIfExists: Boolean = true, attributes:Iterable[FileAttribute[_]]=Nil): Path

  /**
   * Create the directory referenced by this path.
   * <p>
   * If failIfExists then IOException is thrown if the file already exists.
   * In the next Java 7 only version it will throw FileAlreadyExistsException
   * </p>
   * <p>
   * An exception is always thrown if the file is a directory and that directory
   * contains children
   * </p>
   * <p>
   * An exception will also be thrown if the parent directory does not have write
   * permission
   * </p>
   * @param failIfExists
   *          If true and an object exists then an exception will be thrown
   *          If false then the object will be deleted if possible
   *          If not possible to delete the object or it is a non-empty directory
   *            an exception will be thrown
   *          Default is true
   * @param createParents
   *          If true then the containing directories will be created if they do not exist
   *          Default is true
   *
   * @throws IOException
   *           if file or directory already exists.
   *           In the next Java 7 only version it will throw FileAlreadyExistsException
   *           If the process does not have write permission to the parent directory
   *           If parent directory does not exist
   *
   */
  def createDirectory(createParents: Boolean = true, failIfExists: Boolean = true, attributes:Iterable[FileAttribute[_]]=Nil): Path

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
   *  @throws IOException
   *            if the file cannot be written or if there is some other reason
   *            the file cannot be deleted. For example if the file is a non-empty
   *            directory
   */
  def deleteIfExists() = {
    if (exists) {
	delete()
        true
    } else {
      false
    }
  }

  /**
   * Deletes the file or throws an IOException on failure
   *
   * @throws IOException if the file could not be deleted
   */
  def delete(): Unit

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
  def deleteRecursively(continueOnFailure:Boolean=false): (Int,Int)

  // todo
  /**
   *  Copy the underlying object if it exists to the target location.
   *  If the underlying object is a directory it is not copied recursively.
   *
   *  @param target
   *           the target path to copy the filesystem object to.
   *  @param copyAttributes
   *           if true then copy the File attributes of the object
   *           as well as the data.  True by default
   *  @param replaceExisting
   *           if true then replace any existing target object
   *           unless it is a non-empty directory in which case
   *           an IOException is thrown.
   *           False by default
   *
   *  @return
   *           the path to the new copy
   *  @throws IOException
   *           if the copy could not be satisfied because the target could
   *           not be written to or if this path cannot be copied
   */
  def copyTo(target: Path, copyAttributes:Boolean=true,
             replaceExisting:Boolean=false): Path
  /**
   *  Move the underlying object if it exists to the target location.
   *  <p>
   *  If copying of the file is required this will happen, as long as
   *  atomicMove = false.  If atomicMove = true and the move requires copy then
   *  deletion an exception will be thrown.  This is filesystem dependent
   *  </p>
   *
   *  @param target
   *           the target path to move the filesystem object to.
   *  @param replaceExisting
   *           if true then replace any existing target object
   *           unless it is a non-empty directory in which case
   *           an IOException is thrown.
   *           False by default
   *  @param atomicMove
   *           it will guarantee atomicity of the move
   *           False by default
   *
   *  @return true
   *           the path to the moved object
   *  @throws IOException
   *           if the move could not be satisfied because the target could
   *           not be written to or if this path cannot be moved
   */
  def moveTo(target: Path, replaceExisting:Boolean=false,
             atomicMove:Boolean=false): Path

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
   * @param arguments
   *          Arguments to send to the process
   * @param configuration
   *          An optional configuration function for configuring
   *          the ProcessBuilder.  The default process builder will
   *          be passed to the function.
   *
   * @return Process
   */
  def execute(args:String*)(implicit configuration:ProcessBuilder=>Unit = p=>()):Option[Process]

  /**
   * Create a matcher from this path's filesystem
   * @see FileSystem#matcher(String,String)
   */
  def matcher(pattern:String, syntax:String = PathMatcher.StandardSyntax.GLOB) = fileSystem.matcher(pattern, syntax)

  //Directory accessors
  /**
   * An iterable over the contents of the directory.  This is simply walkTree with depth=1.
   * <p>
   * The filter parameter restricts what paths are available through the DirectoryStream.  This is
   * different from using the filter, filterFold or filterEach methods in DirectoryStream because PathMatchers can be used by
   * the underlying filesystem natively and can potentially provide dramatically improved performance for
   * very large directories.
   * </p>
   * 
   * @param filter
   *          A filter that restricts what paths are available in the DirectoryStream
   *          Default is None
   * @return
   *          A managed resource managing a DirectoryStream.
   *
   * @see Path#walkTree
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
  def directoryStream(filter:Option[PathMatcher] = None) : DirectoryStream[Path]

  /**
   * An iterable that traverses all the elements in the directory tree down to the specified depth
   * <p>
   * The filter parameter is a function because the DirectoryStream can return files from many directories.
   * The function provides the mechanism for declaring which PathMatcher to use at each level.  The two
   * parameters are original path and the path to be visited relative to the original path.  By default the
   * function always returns None.
   * </p>
   * <p>
   * If the depth parameter is non-negative then that restricts the depth that will be traversed.  The value 0 will not return any
   * elements, depth = 1 is essentially the {@link path#directoryStream(Option,Boolean)} method and values < 0 will return all elements
   * at any depth.
   * </p>
   * <p>
   * The traversal order is pre-order.
   * </p>
   * <p>
   * No exceptions will be thrown by this method if it is called and the Path is a File or does not exist.  Instead the {@link DirectoryStream}
   * will throw a NotDirectoryException when a method is called and the underlying object is not a Directory.  
   * </p>
   * @param filter
   *          A filter that restricts what paths are available in the DirectoryStream
   *          The directoryStream methods explains why this is often the efficient method
   *          for filtering directories
   *          Default is None
   * @param depth
   *          How deep down the tree to traverse
   *          1 is just visit the object in the directory
   *          >0 is visit all directories in entire tree
   *          Default is 1
   * 
   * @return
   *          A managed resource managing a DirectoryStream.
   *
   * @see Path#directoryStream(Option,Boolean)
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
  def tree(filter:(Path,Path)=>Option[PathMatcher] = (origin,relativePath) => None, 
           depth:Int = -1 /*LinkOption... options*/): DirectoryStream[Path]

  /**
   * Obtains an object for performing reads and writes to files
   *
   * @param codec
   *          The codec representing the encoding of the file
   *          This codec will be the default used for reading and
   *          writing to the file
   *          Default is Codec.default
   */
  def fileOperations(implicit codec: Codec = Codec.default /*LinkOption... options*/):FileOperations


/* ****************** The following require jdk7's nio.file ************************** */

  /*
   * This method is the tree walking anology of secureDirectoryStream for tree.  See
   * both secureDirectoryStream and tree for details.
   * 
   * @param filter
   *          A filter that restricts what paths are available in the DirectoryStream
   *          The directoryStream methods explains why this is often the efficient method
   *          for filtering directories
   *          Default is None
   * @param depth
   *          How deep down the tree to traverse
   *          1 is just visit the object in the directory
   *          >0 is visit all directories in entire tree
   *          Default is 1
   * 
   * @return
   *          A managed resource managing a DirectoryStream.
   *
   * @see Path#secureDirectoryStream(Option)
   * @see Path#tree(Function2, Int)
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
/*
  def secureTree(filter:(Path,Path)=>Option[PathMatcher] = (origin,relativePath) => None, 
                 depth:Int = -1,options:LinkOption*): Option[DirectoryStream[SecuredPath[Path]]]
*/


  /**
   * Attempts to obtain a secured DirectoryStream.  This method and the associated secureTree are intended
   * to be used for security sensitive operations that need to access and/or traverse directory structures
   * in a race free manner.
   * <p>
   * Not all filesystems can support this,  if not then None will be returned.  Using this method will ensure that
   * during the duration of an operation on the {@link DirectoryStream} no external processes
   * are able to modify the directory.
   * </p><p>
   * The stream can also be used as a "virtual" working directory
   * </p>
   * <p>
   * No exceptions will be thrown by this method if it is called and the Path is a File or does not exist.  Instead the {@link DirectoryStream}
   * will throw a NotDirectoryException when a method is called and the underlying object is not a Directory.  
   * </p>
   * <p>
   * <strong>Note:</strong> calling this method does not lock the directory.  Only performing operations on the SecureDirectoryStream will lock the directory.
   * <p>
   * </p>
   * For Example:
   * </p>
   * <pre><code>
   * val tree = path.walkTree()
   * // directory is not yet locked
   * tree.foreach {
   *   // now the directory is locked
   *   println _
   * }
   * // directory has been unlocked
   * </code>
   * <p>
   * @param filter
   *          A filter that restricts what paths are available in the DirectoryStream.
   *          The directoryStream methods explains why this is often the efficient method
   *          for filtering directories
   *          Default is None
   * @return
   *          A managed resource managing a DirectoryStream.
   *
   * @see Path#directoryStream(Option)
   * @see Path#secureTree(Function2,Int)
   */
/*
  def secureDirectoryStream(filter:Option[PathMatcher] = None
                            :LinkOption*): Option[DirectoryStream[SecuredPath[Path]]]
*/
}
