/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import attributes._
import java.io.{IOException, File => JFile}
import java.net.{ URI, URL }
import collection.{ Traversable }

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
   * object 
   */
  implicit def string2path(s: String)(implicit fileSystem: FileSystem = FileSystem.default): Path = apply(s)(fileSystem)
  /**
   * Method to implicitly convert a {@link java.io.File} to a Path
   * object 
   */
  implicit def jfile2path(jfile: JFile)(implicit fileSystem: FileSystem = FileSystem.default): Path = apply(jfile.getPath)(fileSystem)

  /**
   * Enumeration of the Access modes possible for accessing files
   */
  object AccessModes {
    sealed trait AccessMode 
    case object Execute extends AccessMode
    case object Read extends AccessMode
    case object Write extends AccessMode
    def values:Set[AccessMode] = Set(Execute, Read, Write)
  }
  /**
   * Lists the roots of the default filesystem
   */
  def roots: List[Path] = FileSystem.default.roots

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
  def apply(path: String)(implicit fileSystem: FileSystem = FileSystem.default): Path = fileSystem(path)

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
  def apply(jfile: JFile) = FileSystem.default(jfile.getPath)

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
  def createTempFile(prefix: String = FileSystem.default.randomPrefix,
                   suffix: String = null,
                   dir: String = null,
                   deleteOnExit : Boolean = true,
                   attributes:Iterable[FileAttribute[_]] = Nil )
                  (implicit fileSystem: FileSystem = FileSystem.default) : Path = {
    fileSystem.createTempDirectory(prefix,suffix,dir,deleteOnExit)
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
  def createTempDirectory(prefix: String = FileSystem.default.randomPrefix,
                        suffix: String = null,
                        dir: String = null,
                        deleteOnExit : Boolean = true,
                        attributes:Iterable[FileAttribute[_]] = Nil)
                       (implicit fileSystem: FileSystem = FileSystem.default) : Path = {
    fileSystem.createTempDirectory(prefix,suffix,dir,deleteOnExit)
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

  type Closeable = { def close(): Unit }
  private[io] def closeQuietly(target: Closeable) {
    try target.close() catch { case e: IOException => }
  }
  
  private[io] def fail(msg: String) = throw new IOException(msg)
}

import Path.AccessModes._
import Path.fail

/**
 *  A file reference that locates a file using a system independent path.
 *  The file is not required to exist.
 *
 *  @author  Paul Phillips
 *  @author  Jesse Eichar
 *  @since   0.1
 *
 */
abstract class Path (val fileSystem: FileSystem) extends FileOps with PathFinder[Path, PathSet[Path]] with Ordered[Path]
{
  self =>
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
  def toURL: URL = new URL(null,toURI.toString(), fileSystem.urlStreamHandler.orNull)

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
  def /(child: String): this.type
  
  /**
   * Alias for /(String)
   * @see Path#/(String)
   */
  def \(child: String) : this.type = /(child)

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
  def /(child: Path): this.type = /(child.path)

  /**
   * Alias for /(Path)
   * @see Path#/(Path)
   */
  def \(child: Path) : this.type = /(child)

  // identity
  /**
   * The name of the file.  This includes the extension of the file
   * @return the name of the file
   */
  def name: String
  /**
   * The name of the file excluding of the file
   * @return name of the file excluding of the file
   */
  def simpleName: String = name dropRight extension.map{1 + _.size}.getOrElse(0)
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
   def normalize: Path = {
     val Empty = Vector.empty
     val reversedNormalizedPath = (Vector[String]() /: segments) { 
       case (path,".") => path
       case (Empty,"..") => Empty
       case (path,"..") => path dropRight 1
       case (path,seg) => path :+ seg
     }
     val prefix = if(isAbsolute) root.map{_.path}.getOrElse("")
                  else ""
     fileSystem(prefix + (reversedNormalizedPath mkString separator))
   }
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
  def resolve(other: Path): Path = \(other)
  /**
   * Constructs a path from other using the same file system as this
   * path and resolves the this and other in the same manner as
   * {@link Path#resolve(Path)}
   *
   */
  def resolve(other: String): Path = resolve(fileSystem(other))

  /**
   * Make the current path relative to the other path.  If the two paths
   * are on different drives then the other path is returned. If the two
   * paths have different roots the other path is returned.  If the two paths
   * reference the same path then the other path is returned
   * @return relative path from the current path to the other path
   */
  def relativize(other: Path): Path = {
    
    if(other.fileSystem != fileSystem || other.root != root) {
      other
    }else if(segments startsWith other.segments){
      fileSystem(segments.drop(other.segments.size) mkString fileSystem.separator)
    } else {
      null // TODO do we want to relativize this?
    }
  }

  // derived from identity
  /**
   * The root of the file system of the path if it can be determined.
   *
   * @return the root of the file system
   */
  def root: Option[Path] = fileSystem.roots find (this.toAbsolute startsWith _)
  /**
   * The segments in the path including the current element of the path.  If the
   * the path is relative only the segments defined are returned... NOT the absolute
   * path
   * <p>Note segments.last should == name</p>
   *
   * @return the segments in the path
   */
  def segments: List[String] = (path split separator).toList filterNot (_.isEmpty)
  /**
   * The parent path segment if it is possible (for example a root will not have a parent)
   * @return the parent path segment if it possible
   * @see parents
   */
  def parent: Option[Path]
  /**
   * The path segments of the path excluding the current path segment.  The first
   * segment is the first segment in the path. 
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
  def canWrite : Boolean = checkAccess(Write)
  def canRead : Boolean = checkAccess(Read)
  def canExecute : Boolean = checkAccess(Execute)
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
    val x = parent.get \ name
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
   * The size of the file/directory in bytes
   *
   * @return The size of the file/directory in bytes
   * @see java.io.File#length()
   */
  def size: Option[Long]  
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
  
  /**
   * Sets the standard access modes on the underlying path.  If the 
   * underlying object does not exist it will throw an exception.
   * If the underlying system does not support support the mode the mode
   * will be ignored
   * 
   * @param accessModes 
   *          the modes to set on the file in (if possible)
   *          a single atomic update
   */
  def access_=(accessModes:Iterable[AccessMode]) : Unit
  
  /**
   * Short cut for setting the standard access modes on the underlying path.  If the 
   * underlying object does not exist it will throw an exception.
   * If the underlying system does not support support the mode the mode
   * will be ignored
   * 
   * @param accessModes 
   *          string representation of the modes. The standard options
   *          include r - read, w - write, e - execute.  The options are 
   *          filesystem dependent
   */
  def access_=(accessModes:String) : Unit = {
    import Path.fail
    
    val (modifier, modes) = if(accessModes.headOption forall {"+-" contains _}) {
      accessModes.toList.splitAt(1)
    } else {
      (Nil, accessModes.toList)
    }
    
    val actualModes = modes map {
      case 'r' => Read
      case 'w' => Write
      case 'x' => Execute
      case t => fail("access mode %s not recognized" format t)
    }
    
    modifier.headOption match {
      case Some('-') => access = (access -- actualModes)
      case Some('+') => access = (access ++ actualModes)
      case None => access = actualModes
    }
  }
  /**
   * Reads the access modes from the file and returns the Set
   * This does not lock the file so the modes could be out of date
   * even by the time the method returns if used in a heavily
   * parallel environment
   *
   * @return the access modes set on the file
   */
  def access : AccessSet = new AccessSet(this)
  
  def attributes : Iterable[FileAttribute[_]] = {
    List(LastModifiedAttribute(lastModified),
         ReadAccessAttribute(canRead),
         WriteAccessAttribute(canWrite),
         ExecuteAccessAttribute(canExecute))

  }
  def attributes_= (attrs:Iterable[FileAttribute[_]]) = {
    attrs.foreach {
      case LastModifiedAttribute(newLastModified) => lastModified = newLastModified
      case ReadAccessAttribute(newRead) => access(Read) = newRead
      case WriteAccessAttribute(newWrite) => access(Write) = newWrite
      case ExecuteAccessAttribute(newExecute) => access(Execute) = newExecute
      case _ => ()
    }
  }

  // creations
  private def createContainingDir(createParents: Boolean) = {
    def testWrite(parent:Option[Path]) = {
      parent match {
        case Some(p) if !p.canWrite => fail("Cannot write parent directory: "+p+" of "+ path)
        case Some(p) if !p.isDirectory => fail("parent path is not a directory")
        case Some(p) if !p.canExecute => fail("Cannot execute in parent directory: "+p+" of "+ path)
        case Some(p) => ()
        case None => fail("Parent directory cannot be created: '"+path+"'")
      }
    }


    (createParents, parent) match {
      case (_, Some(p)) if p.exists =>
        testWrite(parent)
      case (true, Some(_)) => 
        doCreateParents()
        testWrite(parent)
      case (true, None) =>
        doCreateParents()
        testWrite(toAbsolute.parent)
      case (false, _) =>
        fail("Parent directory of "+this+" does not exist and cannot be created")      
    }
  }
  
  /**
   * NOT PUBLIC API: Create all parent directories of the current Path
   */
  protected def doCreateParents():Unit
  
  /**
   * NOT PUBLIC API: Create a directory for the current path without considering if the parents 
   * has been previously created.   This method should fail if the parent does not exist
   */
  protected def doCreateDirectory():Boolean
  /**
   * NOT PUBLIC API: Create a file for the current path without considering if the parents 
   * has been previously created.   This method should fail if the parent does not exist
   */
  protected def doCreateFile():Boolean  
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
   * An Exception will also be thrown if the parent directory does not have write
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
   * @param accessModes
   *          The access modes that to set on the file
   *          Default is Read,Write
   * @param attributes
   *          Filesystem specific attributes to apply to the file
   *          Ignored unless on Java 7+ JVM
   * @throws IOException
   *           If file or directory already exists.
   *           In the next Java 7 only version it will throw FileAlreadyExistsException
   *           If the process does not have write permission to the parent directory
   *           If parent directory does not exist
   */
  def createFile( createParents:Boolean = true, failIfExists: Boolean = true, 
                 accessModes:Iterable[AccessMode]=List(Read,Write), attributes:Iterable[FileAttribute[_]]=Nil): Path = {

    exists match {
      case true if failIfExists =>
        fail("File '%s' already exists." format name)
      case true if isDirectory =>
        fail("Path "+this+" is a directory and thus cannot be created as a file")
      case true =>
        this.attributes = attributes
        access = accessModes
      case _ =>
      createContainingDir (createParents)
      // next assertions should be satisfied by createContainingDir or have already thrown an exception
      assert(parent.forall{p => p.exists}, "parent must exist for a file to be created within it")
      assert(parent.forall{p => p.canWrite}, "parent must be writeable")
      assert(parent.forall{p => p.isDirectory}, "parent must be executable")
      assert(parent.forall{p => p.canExecute}, "parent must be executable")

      val res = doCreateFile()
      access = accessModes
      if (!res) fail("unable to create file")
    }
    this
  }

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
   * @param createParents
   *          If true then the containing directories will be created if they do not exist
   *          Default is true
   * @param failIfExists
   *          If true and an object exists then an exception will be thrown
   *          If false then the object will be deleted if possible
   *          If not possible to delete the object or it is a non-empty directory
   *            an exception will be thrown
   *          Default is true
   * @param accessModes
   *          The access modes that to set on the file
   *          Default is Read,Write
   * @param attributes
   *          Filesystem specific attributes to apply to the file
   *          Ignored unless on Java 7+ JVM
   *
   * @throws IOException
   *           if file or directory already exists.
   *           In the next Java 7 only version it will throw FileAlreadyExistsException
   *           If the process does not have write permission to the parent directory
   *           If parent directory does not exist
   *
   */
  def createDirectory(createParents: Boolean = true, failIfExists: Boolean = true,
                      accessModes:Iterable[AccessMode]=List(Read,Write,Execute), attributes:Iterable[FileAttribute[_]]=Nil) =  {
    if (failIfExists && exists) fail("Directory '%s' already exists." format name)
    if (exists && isFile) fail("Path "+this+" is a fileand thus cannot be created as a directory")
    
    if(root != Some(this) && notExists) {
      createContainingDir (createParents)

      val res = doCreateDirectory()
    }
    this.attributes = attributes
    access = accessModes
    this
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
   * @param force
   *          if the file is write protected force will override 
   *          the write protection and delete the file.  If not
   *          force then an IOException will be thrown indicating
   *          failure of deletion.
   *          Default is false
   *  @throws IOException
   *            if the file cannot be written or if there is some other reason
   *            the file cannot be deleted. For example if the file is a non-empty
   *            directory
   */
  def deleteIfExists(force : Boolean = false) = {
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
   * @param force
   *          if the file is write protected force will override 
   *          the write protection and delete the file.  If not
   *          force then an IOException will be thrown indicating
   *          failure of deletion.
   *          Default is false
   * @return this
   * @throws IOException if the file could not be deleted
   */
  def delete(force : Boolean = false): Path

  /**
   *  Deletes the directory recursively.
   * <p>
   *  This method does not detect circular directory graphs and
   *  does not promise to perform the delete in an atomic operation
   * </p>
   *  <p>Use with caution!</p>
   * @param force
   *          if the file is write protected force will override 
   *          the write protection and delete the file.  If not
   *          force then an IOException will be thrown indicating
   *          failure of deletion.
   *          Default is false
   *  @param continueOnFailure
   *           If false then method will throw an exception when encountering a
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
  def deleteRecursively(force : Boolean = false, continueOnFailure:Boolean=false): (Int,Int) = {
    if (isDirectory) {
      var successes = 0
      var failures = 0
      
      def tryDelete(p:Path) = {
        try {
          p.delete(force)
          successes += 1
        } catch {
          case e:IOException if continueOnFailure =>
          failures += 1
        }
        (successes, failures)
      }
      
      children{_.isFile} foreach tryDelete
      children{_.isDirectory} foreach { path =>
        val (deleted, remaining) = path.deleteRecursively(force,continueOnFailure)
        successes += deleted
        failures += remaining
      }
      tryDelete(this)
    } else {
      try {
        delete(force)
        (1,0)
      } catch {
        case e:IOException if continueOnFailure =>
        (0,1)
      }
    }
  }

  /**
   *  Copy the underlying object if it exists to the target location.
   *  If the underlying object is a directory it is not copied recursively.
   *
   *  @param target
   *           the target path to copy the filesystem object to.
   *  @param copyAttributes
   *           if true then copy the File attributes of the object
   *           as well as the data.  True by default
   *  @param depth
   *           The depth of the copy if the path is a Directory.
   *           default is entire tree
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
  def copyTo(target: Path, 
             createParents : Boolean=true,
             copyAttributes : Boolean=true,
             replaceExisting : Boolean=false): Path = {

  	if (this.normalize == target.normalize) return target

    if (!createParents && target.parent.map(_.notExists).getOrElse(true)) fail("Parent directory of destination file does not exist.")
    if (target.exists && !replaceExisting) fail("Destination file already exists, force creation or choose another file.")
    if (target.exists && !target.checkAccess(Write)) fail("Destination exists but is not writable.")
    if (target.isDirectory && target.children().nonEmpty) fail("Destination exists but is a non-empty directory.")
    if (replaceExisting) target.deleteIfExists()


    if (isDirectory) target.createDirectory(createParents, false)
    else {
      if(createParents) {
        target.parent.foreach{_.createDirectory(true,false)}
      }
      copyFile(target)
    }

    if(copyAttributes) {
      target.attributes = attributes
    }
    target
  }

  /**
   * Copy a the contents of a Path representing a file to a new destination
   * a check has been performed that the file exists, so barring a race condition with
   * another thread or process this path does exist and is a file.
   * Dest will not exist at the time of this call.
   */
  protected def copyFile(dest: Path): Path

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
  def moveTo(target: Path, replace:Boolean=false,
             atomicMove:Boolean=false) : Path = {

    if(fileSystem.roots.contains(this)) {
       throw new IOException("cannot move a root path")
    }

    if( target.exists && replace) {
      target match {
        case _ if target.isDirectory && target.children().nonEmpty => fail("can not replace a non-empty directory: "+target)
        case _ => target.delete()
      }
    }

    import Matching._

   this match {
       case NonExistent(_) => fail("attempted to move "+path+" but it does not exist")
       case _ if target.normalize == normalize => ()
       case _ if !replace && target.exists => fail(target+" exists but replace parameter is false")
       case File(_) if target.isDirectory => fail("cannot overwrite a directory with a file")
       case Directory(_) if target.isFile => fail("cannot overwrite a file with a directory")
       // TODO move between two fileSystems
       case File(_) if target.fileSystem != fileSystem =>
         target write bytesAsInts
         delete()
       case _ if target.fileSystem != fileSystem && this.isDirectory =>
         val x = target.exists
         target.createDirectory()
         val z = descendants() forall {_.exists}
         children() foreach { path =>
             path moveTo (target \ path.relativize(self))
         }
         delete()
       case File(_) if target.notExists || target.isFile => moveFile(target,atomicMove)
       case Directory(_) if target.notExists => moveDirectory(target,atomicMove)
       case _ => throw new Error("not yet handled "+target+","+this)
     }
     target
  }

  /**
   * Called to move the current file to another location <strong>on the same filesystem</strong>
   */
  protected def moveFile(target:Path, atomicMove : Boolean) : Unit
  /**
   * Called to move the current directory to another location <strong>on the same filesystem</strong>
   */
  protected def moveDirectory(target:Path, atomicMove : Boolean) : Unit

  override def toString() = "Path(%s)".format(path)
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }
  override def hashCode() = path.hashCode()

  /**
   * Create a matcher from this path's filesystem
   * @see FileSystem # matcher ( String, String )
   */
  def matcher(pattern:String, syntax:String = PathMatcher.StandardSyntax.GLOB) = fileSystem.matcher(pattern, syntax)

  //Directory accessors
  /**
   * An iterable over the contents of the directory.  This is simply walkTree with depth=1.
   * <p>
   * The filter parameter restricts what paths are available through the PathSet.  This is
   * different from using the filter, filterFold or filterEach methods in PathSet because PathMatchers can be used by
   * the underlying filesystem natively and can potentially provide dramatically improved performance for
   * very large directories.
   * </p>
   * 
   * @param filter
   *          A filter that restricts what paths are available in the PathSet
   *          If the filter is a PathMatcher and the underlying filesystem supports the PatchMatcher
   *          implementation then the maximum performance will be achieved.
   *          All Paths that are passed to matcher is relative to this Path
   *          Default is PathMatcher.ALL
   * @return
   *          A managed resource managing a PathSet.
   *
   * @see Path#walkTree
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
   def children(filter:Path => Boolean = PathMatcher.ALL, options:Traversable[LinkOption]=Nil) : PathSet[Path] = descendants(filter, depth=1, options)

  /**
   * An iterable that traverses all the elements in the directory tree down to the specified depth
   * <p>
   * The filter parameter is a function because the PathSet can return files from many directories.
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
   * No exceptions will be thrown by this method if it is called and the Path is a File or does not exist.  Instead the {@link PathSet}
   * will throw a NotDirectoryException when a method is called and the underlying object is not a Directory.  
   * </p>
   * @param filter
   *          A filter that restricts what paths are available in the PathSet
   *          If the filter is a PathMatcher and the underlying filesystem supports the PatchMatcher
   *          implementation then the maximum performance will be achieved.
   *          All Paths that are passed to matcher is relative to this Path
   *          Default is PathMatcher.ALL
   * @param depth
   *          How deep down the tree to traverse
   *          1 is just visit the objects in the directory
   *          negative values will visit entire tree
   *          Default is -1
   * 
   * @return
   *          A managed resource managing a PathSet.
   *
   * @see Path#directoryStream(Option,Boolean)
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
  def descendants(filter:Path => Boolean = PathMatcher.ALL, 
           depth:Int = -1, options:Traversable[LinkOption]=Nil): PathSet[Path]


/* ****************** The following require jdk7's nio.file ************************** */

  /*
   * This method is the tree walking anology of secureDirectoryStream for tree.  See
   * both secureDirectoryStream and tree for details.
   * 
   * @param filter
   *          A filter that restricts what paths are available in the PathSet
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
   *          A managed resource managing a PathSet.
   *
   * @see Path#secureDirectoryStream(Option)
   * @see Path#tree(Function2, Int)
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
/*
  def secureTree(filter:(Path,Path)=>Option[PathMatcher] = (origin,relativePath) => None, 
                 depth:Int = -1,options:LinkOption*): Option[PathSet[SecuredPath[Path]]]
*/


  /**
   * Attempts to obtain a secured PathSet.  This method and the associated secureTree are intended
   * to be used for security sensitive operations that need to access and/or traverse directory structures
   * in a race free manner.
   * <p>
   * Not all filesystems can support this,  if not then None will be returned.  Using this method will ensure that
   * during the duration of an operation on the {@link PathSet} no external processes
   * are able to modify the directory.
   * </p><p>
   * The stream can also be used as a "virtual" working directory
   * </p>
   * <p>
   * No exceptions will be thrown by this method if it is called and the Path is a File or does not exist.  Instead the {@link PathSet}
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
   *          A filter that restricts what paths are available in the PathSet.
   *          The directoryStream methods explains why this is often the efficient method
   *          for filtering directories
   *          Default is None
   * @return
   *          A managed resource managing a PathSet.
   *
   * @see Path#directoryStream(Option)
   * @see Path#secureTree(Function2,Int)
   */
/*
  def secureDirectoryStream(filter:Option[PathMatcher] = None
                            :LinkOption*): Option[PathSet[SecuredPath[Path]]]
*/

  // Optimization for Seekable
  override protected def tempFile() : Path = fileSystem.createTempFile()
  
}
