/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file

import java.io.{File => JFile}
import java.nio.file.{Path => JPath, Files => JFiles, Paths => JPaths}
import java.nio.file.attribute.{PosixFilePermission}
import java.io.IOException
import java.net.URI
import java.net.URL
import scala.collection.Traversable
import PathMatcher.IsDirectory
import PathMatcher.IsFile
import PathMatcher.NonExistent
import scalax.io.CloseableIterator
import scalax.io.Input
import scalax.io.Output
import java.nio.file.attribute.PosixFilePermission
import scalax.io.StandardOpenOption

/**
 * The object for constructing Path objects and for containing implicits from strings and
 * {@link java.file.File}s to Scala paths.
 * <p> All Paths constructed by this factory are created for the default filesystem</p>
 *
 *  @author Jesse Eichar
 *  @since  0.1
 */
object Path
{
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
  def roots: Set[Path] = FileSystem.default.roots

  /**
   * Create a Path from a string
   *
   * @param path
   *          the string to use for creating the Path
   */
  def fromString(path: String): Path = FileSystem.default.fromString(path)
  def apply(path: String*): Path = FileSystem.default(path:_*)
  def apply(pathRepresentation: String, separator:Char): Path = FileSystem.default(pathRepresentation, separator)

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
  def apply (uri: URI): Option[Path] = {
    Option(JPaths.get(uri)).map {jpath =>
      new Path(jpath, new FileSystem(jpath.getFileSystem))
    }
  }

  /**
   * Create a Path on the default files system from a {@link java.file.File}
   *
   * @param jfile
   *          the file to use for creating the Path
   */
  def apply(jfile: JFile): Path = FileSystem.default.fromString(jfile.getPath)

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
   */
  def createTempFile(prefix: String = FileSystem.default.randomPrefix,
                   suffix: Option[String] = None,
                   dir: Option[String] = None,
                   deleteOnExit : Boolean = true,
                   attributes:Set[FileAttribute[_]] = Set.empty ): Path = {
    FileSystem.default.createTempFile(prefix,suffix,dir,deleteOnExit, attributes)
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
                   dir: Option[String] = None,
                   deleteOnExit : Boolean = true,
                   attributes:Set[FileAttribute[_]] = Set.empty ): Path = {
    FileSystem.default.createTempDirectory(prefix,dir,deleteOnExit, attributes)
  }

  /**
   * Allows matching on the path segments of a Path
   * {{{
   * Path("c:","dir", "file")
   * }}}
   * both of the above examples will match the same path
   *
   * For more on matching Paths see [[scalax.file.PathMatcher]]
   */
  def unapplySeq(pathExpr:Path): Option[Seq[String]] = Some(pathExpr.segments)

  private[file] def fail(msg: String) =
    throw new IOException(msg)
}



/**
 *  A file reference that locates a file using a system independent path.
 *  The file is not required to exist.
 *
 *  @author  Paul Phillips
 *  @author  Jesse Eichar
 *  @since   0.1
 *
 */
class Path private[file] (val jpath:JPath, val fileSystem: FileSystem) extends FileOps with PathFinder[Path] with Ordered[Path]
{
  self =>
  import Path._
  import Path.AccessModes._

  protected def assertExists = if(!exists) throw new java.io.FileNotFoundException(path+" does not exist")

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
  def toAbsolute: Path =if (isAbsolute) this else new Path(jpath.toAbsolutePath, fileSystem)
  /**
   * The true/real representation of the current path.
   *
   * The full and true path of this path will be resolved, links will be handled according to the link options
   * . and .. etc... will be resolved and if this path is relative it will be made absolute.
   *
   * If no linkOptions are supplied this method will follow links
   *
   * @note in Java 6 linkOptions are ignored because only Java.io.File apis are used but in Java 7 linkOptions are
   * correctly handled
   *
   * @param linkOptions How to handle link options
   *
   * @return the ''real'' path
   */
  def toRealPath(linkOptions:LinkOption*): Path = new Path(jpath.toRealPath(linkOptions:_*), fileSystem)
  
  /**
   * If this path is a symbolic link to another file then return the other end of the link
   * 
   * @return the path at the other end of the link or none if this path is not a symbolic link
   */
  def readSymbolicLink = if (isSymboliclink) Some(new Path(JFiles.readSymbolicLink(jpath), fileSystem)) else None
  /**
   * Return a java.io.File if possible
   */
  def toFile:Option[java.io.File] = Option(jpath.toFile)
  /**
   * Creates a URI from the path.  The uri will be the absolute path
   * @see java.file.File#toURI
   */
  def toURI: URI = jpath.toUri
  /**
   * Creates a URL from the path.  This does have the bug present in {@link java.file.File#toURL}
   * and can be used directly.
   * @see java.file.File#toURI
   */
  def toURL: URL = toURI.toURL

  /**
   * If child is relative, creates a new Path based on the current path with the
   * child appended. If child is absolute the child is returned
   *
   *<
   * <ul>
   * <li>if other is null return this</li>
   * <li>if other is absolute return other</li>
   * <li>if other is not absolute the return this append other</li>
   * </ul>
   * <strong>@note child is a single child if it contains a path separator it will NOT be considered a separator character</strong>
   *
   * <p>Examples include:
   * <pre><code>
   * path / "child" / "grandchild"
   * path / ".." / "sibling"
   * </code></pre>
   * </p><p>
   * Illegal examples include;
   * <pre><code>
   * path / "child/grandchild"
   * path / "../sibling"
   * </code></pre>
   * In these cases an exception will be thrown
   * </p>
   *
   * @return A new path with the specified path appended
   *
   * @see #\(String)
   */
  def /(child: String): Path = {
    fileSystem.checkSegmentForSeparators(child)
    fileSystem(jpath.resolve(child))
  }

  /**
   * Add several children to this path.  The sep character will be used to split the path string,
   * A path will be created from the resulting sequence and finally all children will be added to this path.
   *
   * Examples:
   *
   * {{{
   * path / ("a,c,d,e", ',') // results in path / a / c / d / e
   * path / ("/a/b/c/d/", '/') // results in path / a / b / c / d
   * path / ("//../a","/") // results in path / .. / a
   *
   * path / ("/a",',') // results in an exception if / == Path.separator
   * path / ("//",'/') // returns same Path
   * }}}
   */
  def /(pathRepresentation: String, separator:Char): Path = /(fileSystem.fromSeq(pathRepresentation.split(separator)))

  /**
   * Alias for /(String)
   *
   * @see /(String)
   */
  def \(child: String): Path = /(child)
  def \(pathRepresentation: String, separator:Char): Path = /(pathRepresentation,separator)
  /**
   * Alias for /(child.name)
   *
   * @return A new path with the specified path appended
   * @see #/(String)
   */
  final def /(child: Path): Path = new Path(jpath.resolve(child.jpath), fileSystem)

  /**
   * Alias for /(Path)
   * @see #/(Path)
   */
  final def \(child: Path) : Path = /(child)

  // identity
  /**
   * The name of the file.  This includes the extension of the file
   * @return the name of the file
   */
  def name: String = Option(jpath.getFileName).map(_.toString) getOrElse ""
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
  def path: String = jpath.toString
  /**
   * Returns the related Path that starts at a root of the file system and is the direct
   * path with all relative segments are resolved.
   *
   * For example /home/user/../another is <em>not</em> a valid normalized path.
   *
   * @see #toAbsolute
   * @see java.file.File#toCanonical
   */
   lazy val normalize: Path = {
     val Empty = Vector.empty
     val reversedNormalizedPath = (Vector[String]() /: segments) {
       case (path,".") => path
       case (Empty,"..") => Empty
       case (path,"..") => path dropRight 1
       case (path,seg) => path :+ seg
     }
/*     val prefix = if(isAbsolute) root.map{_.path}.getOrElse("")
                  else ""
     if(reversedNormalizedPath startsWith prefix) {*/
       fileSystem.fromSeq(reversedNormalizedPath)
     /*} else {
       fileSystem.fromSeq(prefix +: reversedNormalizedPath)
     }*/
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
   * Examples:
   * {{{
   * path resolve ("a/b/c",'/') // result is Path / a / b / c
   * path resolve ("//..//b//",'/') // result is Path / .. / b
   * }}}
   * @param other the string representation of the path with segment separators as indicated by separator
   * @param separator the separator character used in other
   * @return a path resolved as a child of this
   */
  def resolve(other: String, separator:Char): Path = resolve(fileSystem(other,separator))

  /**
   * Constructs a path from other using the same file system as this
   * path and resolves the this and other in the same manner as
   * {@link Path#resolve(Path)}
   *
   * Examples:
   * {{{
   * path resolve ("a/b/c",'/') // result is Path / a / b / c
   * path resolve ("//..//b//",'/') // result is Path / .. / b
   * }}}
   * @param pathSegments the path segments that make up the path
   * @return a path resolved as a child of this
   */
  def resolve(pathSegments: String*): Path = resolve(fileSystem(pathSegments:_*))

  /**
   * Make the current path relative to the other path.  If the two paths
   * are on different drives then the other path is returned. If the two
   * paths have different roots the other path is returned.  If the two paths
   * reference the same path then the other path is returned.
   * 
   * For example:
   * 
   * this = /a/b
   * other = /a/b/c/d
   * 
   * this relativize other = c/d
   * 
   * other relative this = ../..
   * 
   * @return relative path from the current path to the other path
   */
  def relativize(other: Path): Path = {
    if (fileSystem != other.fileSystem) other
    else new Path(jpath.relativize(other.jpath), fileSystem)
  }

  // derived from identity
  /**
   * The root of the file system of the path if it can be determined.
   *
   * @return the root of the file system
   */
  lazy val root: Option[Path] = {
    val absolute = toAbsolute // cache so don't need to recalculate
    val startsWith = absolute startsWith _
    fileSystem.cachedRoots.find(startsWith) orElse (fileSystem.roots find (startsWith))
  }
  /**
   * The segments in the path including the current element of the path.  If the
   * the path is relative only the segments defined are returned... NOT the absolute
   * path
   * <p>Note segments.last should == name</p>
   *
   * @return the segments in the path
   */
  lazy val segments: Seq[String] = new Seq[String] {
    override def length = jpath.getNameCount
    override def apply(i: Int) = jpath.getName(i).toString
    override def iterator = new Iterator[String]{
      var i = {
        if(jpath.toString startsWith separator) -1
        else 0
      }
      def hasNext = i < jpath.getNameCount
      def next = {
        i += 1
        if(i == 0) {
          separator
        } else {
          jpath.getName(i-1).toString
        }
      }
    }
  }
  /**
   * Resolves other against this path's parent in the same manner as in resolve(Path).
   *
   *
   * If parent does not exist fileSystem.fromSeq(other.segments) will be returned.  Otherwise parent.resolve(other) will be returned
   *
   * @param other the path from parent to the sibling.
   * @return a path resolved as a child of parent or None if parent is None
   */
  def sibling(other:Path):Path = {
    parent map (_.resolve(other).asInstanceOf[Path]) getOrElse (fileSystem.fromSeq(other.segments))
  }

  /**
   * Resolves other against this path's parent in the same manner as sibling(Path)
   *
   * @param other the path from parent to the sibling.
   * @param separator the separator character that is used in other
   * @return a path resolved as a child of parent or other if there is no parent
   */
  def sibling(other:String, separator:Char):Path = sibling(fileSystem(other,separator))

  /**
   * Resolves other against this path's parent in the same manner as sibling(Path)
   *
   * @param pathSegments the path from parent to the sibling.
   * @return a path resolved as a child of parent or other if there is no parent
   */
  def sibling(pathSegments:String*):Path = sibling(fileSystem(pathSegments:_*))

  /**
   * The parent path segment if it is possible (for example a root will not have a parent)
   * @return the parent path segment if it possible
   * @see parents
   */
  def parent: Option[Path] = Option(jpath.getParent()) map (jp => new Path(jp,fileSystem))

  /**
   * The path segments of the path excluding the current path segment.  The first
   * segment is the first segment in the path.
   * @return The path segments of the path excluding the current path segment
   * @see segments
   */
  lazy val parents: Seq[Path] = parent match {
    case None     => Vector.empty
    case Some(p)  => p +: p.parents
  }
  /**
   * The extension of the name of the path, if it exists. if name ends with an
   * extension (e.g. "foo.jpg") returns the extension ("jpg")
   *
   * @return the extension of the path
   */
  lazy val extension: Option[String] =
    name.lastIndexWhere (_ == '.') match {
      case idx if idx != -1 => Some(name.drop(idx + 1))
      case _ => None
    }

  // Boolean tests
  /**
   * Check if the referenced file both exists and be accessed with the requested modes
   *
   * @param modes the modes to check for on the file.  If empty then only existance
   *        is checked
   * @return true if all modes are available on the file
   */
  def checkAccess(modes: AccessMode*): Boolean = {
    modes forall {
      case Execute  => JFiles.isExecutable(jpath)
      case Read     => JFiles.isReadable(jpath)
      case Write    => JFiles.isWritable(jpath)
    }
  }

  /**
   * Check modes using the rwx characters.  The string can be from 1-3 characters long and can be r w x in any order.
   * Spaces are ignored
   */
  def checkAccess(modes: String): Boolean = {
    val (access, garbage) = modes.replaceAll("\\s","").toLowerCase.partition("rwx".toList.contains)
    require(garbage.isEmpty, "characters "+garbage+"are not legal options")

    checkAccess (access map {
        case 'r' => Read
        case 'w' => Write
        case 'x' => Execute
      } :_*)
  }
  def canWrite : Boolean = JFiles.isWritable(jpath)
  def canRead : Boolean = JFiles.isReadable(jpath)
  def canExecute : Boolean = JFiles.isExecutable(jpath)
  /**
   * True if the path exists in the file system
   * 
   * @param linkOptions control how to follow symbolic links
   * 					default is to follow symbolic links 
   * @return true if the path exists in the file system
   * @see java.file.File#exists
   */
  def exists(implicit linkOptions:Seq[LinkOption] = Seq.empty): Boolean = JFiles.exists(jpath, linkOptions:_*)
  /**
   * False if the path does not exist in the file system
   * 
   * @param linkOptions control how to follow symbolic links
   * 					default is to follow symbolic links 
   * @return false if the path does not exist in the file system
   */
  def nonExistent(implicit linkOptions:Seq[LinkOption] = Seq.empty) = !JFiles.exists(jpath, linkOptions:_*)
  /**
   * True if the path exists and is a file
   *
   * @param linkOptions control how to follow symbolic links
   * 					default is to follow symbolic links 
   * @return true if the path exists and is a file
   * @see java.file.File#isFile
   */
  def isFile(implicit linkOptions:Seq[LinkOption] = Seq.empty): Boolean = JFiles.isRegularFile(jpath, linkOptions:_*)
  /**
   * True if the path exists and is a directory. 
   *
   * @param linkOptions control how to follow symbolic links 
   * 					default is to follow symbolic links 
   * @return true if the path exists and is a directory
   * @see java.file.File#isDirectory
   */
  def isDirectory(implicit linkOptions:Seq[LinkOption] = Seq.empty): Boolean = JFiles.isDirectory(jpath, linkOptions:_*)
  /**
   * True is the file is absolute.
   * IE is rooted at a filesystem root
   * @return true if file is absolute.
   * @see java.file.File#isAbsolute
   */
  def isAbsolute: Boolean = jpath.isAbsolute()
  /**
   * True if the file is a hidden file for the current
   * filesystem
   *
   * @return True if the file is a hidden file for the current
   * filesystem
   * @see java.file.File#isHidden()
   */
  def isHidden: Boolean = JFiles.isHidden(jpath)
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
  def isSymboliclink = JFiles.isSymbolicLink(jpath)

  // Information
  /**
   * The time of the last modification of the file
   *
   * @return the time modified or -1 if not applicable for fileSystem
   * @see java.file.File#getLastModified()
   */
  def lastModified:FileTime = JFiles.getLastModifiedTime(jpath)
  /**
   * Set the last time modified of the file
   *
   * @return the new time
   * @see java.file.File#setLastModified(Long)
   */
  def lastModified_=(time: FileTime): FileTime = {JFiles.setLastModifiedTime(jpath, time.jfileTime); time}
  /**
   * The size of the file/directory in bytes
   *
   * @return The size of the file/directory in bytes
   * @see java.file.File#length()
   */
  def size: Option[Long] = if(exists) Some(JFiles.size(jpath)) else None
  // Boolean path comparisons
  /**
   * True if this path ends with the other path
   * @return True if this path ends with the other path
   */
  def endsWith(other: Path):Boolean = jpath.endsWith(other.jpath)
  /**
   * True if this path starts with the other path
   * @return True if this path starts with the other path
   */
  def startsWith(other: Path):Boolean = jpath.startsWith(other.jpath)
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
  def isSame(other: Path): Boolean = JFiles.isSameFile(jpath, other.jpath)
  /**
   * True if this path has been modified more recently than other.
   * If this file does not exist it is not fresh than other
   *
   * @return True if this path has been modified more recently than other.
   */
  def isFresher(other: Path): Boolean = lastModified.toMillis > other.lastModified.toMillis
  /**
   * Compares this path to the other lexigraphically.
   */
  def compare(other:Path):Int = jpath.compareTo(other.jpath)

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
  def access_=(accessModes:Iterable[AccessMode]) : Unit = {
    if (nonExistent) fail("Path %s does not exist".format(path))
    if (attributes.supportsView[PosixFileAttributeView]) {
    	val permissions = accessModes.flatMap {
    	case Write => PosixFilePermission.GROUP_WRITE :: PosixFilePermission.OTHERS_WRITE :: PosixFilePermission.OWNER_WRITE :: Nil
    	case Read => PosixFilePermission.GROUP_READ :: PosixFilePermission.OTHERS_READ :: PosixFilePermission.OWNER_READ :: Nil
    	case Execute => PosixFilePermission.GROUP_EXECUTE :: PosixFilePermission.OTHERS_EXECUTE :: PosixFilePermission.OWNER_EXECUTE:: Nil
    	case _ => Nil
    	}
    	import collection.JavaConverters._
    	JFiles.setPosixFilePermissions(jpath, permissions.toSet.asJava)
    } else if (attributes.supportsView[DosFileAttributeView]) {
      attributes.view[DosFileAttributeView].get.setReadOnly(!accessModes.exists(_ == Write))
    }
  }

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
      case Some(c) => throw new IllegalArgumentException("Only + and - are acceptable mode modifiers.  Found: "+c)
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

  private val fileAttribute = new FileAttributes(this)
  /**
   * Return the FileAttributes object for advanced reading and setting 
   * of the file's attributes
   */
  def attributes : FileAttributes = fileAttribute
  def attributes_=(newAttributes: TraversableOnce[FileAttribute[_]]) : this.type = {
    newAttributes.foreach(att => fileAttribute.update(att))
    this
  }
  
  // creations
  private def createContainingDir(createParents: Boolean) = {
    def testWrite(parent:Option[Path]) = {
      parent match {
        case Some(p) if !p.canWrite => fail("Cannot write to parent directory: "+p+" of "+ path)
        case Some(p) if !p.isDirectory => fail("parent path is not a directory")
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
  private def doCreateParents():Unit = Option(jpath.toAbsolutePath.getParent()).foreach(f => JFiles.createDirectories(f))

  /**
   * NOT PUBLIC API: Create a directory for the current path without considering if the parents
   * has been previously created.   This method should fail if the parent does not exist
   */
  private def doCreateDirectory():Unit = JFiles.createDirectory(jpath.toAbsolutePath)
  /**
   * NOT PUBLIC API: Create a file for the current path without considering if the parents
   * has been previously created.   This method should fail if the parent does not exist
   */
  private def doCreateFile():Unit = JFiles.createFile(jpath.toAbsolutePath)
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
   *          Filesystem specific attributes to fromString to the file
   *          Ignored unless on Java 7+ JVM
   * @throws IOException
   *           If file or directory already exists.
   *           In the next Java 7 only version it will throw FileAlreadyExistsException
   *           If the process does not have write permission to the parent directory
   *           If parent directory does not exist
   */
// TODO Exceptions
  def createFile( createParents:Boolean = true, failIfExists: Boolean = true,
                 accessModes:Iterable[AccessMode]=List(Read,Write), attributes:Iterable[FileAttribute[_]]=Nil): Path = {

    exists match {
      case true if failIfExists =>
        fail("File '%s' already exists." format name)
      case true if isDirectory =>
        fail("Path "+this+" is a directory and thus cannot be created as a file")
      case true =>
        attributes.foreach{att => this.attributes(att.name) = att.value}
        access = accessModes
      case _ =>
      createContainingDir (createParents)
      // next assertions should be satisfied by createContainingDir or have already thrown an exception
      assert(parent.forall{p => p.exists}, "parent must exist for a file to be created within it")
      assert(parent.forall{p => p.canWrite}, "parent must be writeable")
      assert(parent.forall{p => p.isDirectory}, "parent must be executable")

      doCreateFile()
      access = accessModes
    }
    this.asInstanceOf[Path]
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
   *          Filesystem specific attributes to fromString to the file
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
                      accessModes:Iterable[AccessMode]=List(Read,Write,Execute), attributes:Iterable[FileAttribute[_]]=Nil):Path =  {
	  val exsts = exists(Seq(LinkOptions.NoFollowLinks))
    if (failIfExists && exsts) fail("Directory '%s' already exists." format name)
    val notDir = !isDirectory
    if (exsts && notDir) {
      fail("Path "+this+" is a file (or symlink) and thus cannot be created as a directory")
    }

    if (exsts) {
      this
    } else {
      if (root != Some(this) && nonExistent) {
        createContainingDir(createParents)

        val res = doCreateDirectory()
      }
      attributes.foreach { att => this.attributes(att.name) = att.value }

      access = accessModes
      this
	}
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
      delete(force)
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
  def delete(force : Boolean = false): this.type = {
    if(exists) {
      if (force) access_= (access + Write)

      if(!canWrite) fail("File is not writeable so the file cannot be deleted")
      JFiles.delete(jpath)
    }
    this
  }

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
    if (exists && isDirectory) {
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

      children{(_:Path).isFile} foreach tryDelete
      children{(_:Path).isDirectory} foreach { path =>
        val (deleted, remaining) = path.deleteRecursively(force,continueOnFailure)
        successes += deleted
        failures += remaining
      }
      tryDelete(this)
    } else if (exists) {
      try {
        delete(force)
        (1,0)
      } catch {
        case e:IOException if continueOnFailure =>
        (0,1)
      }
    } else {
      (0,0)
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
   *           A depth of 0 means only the current Path is copied
   *           A depth of 1 means all children are copied as well, etc...
   *           default is entire tree
   *  @param replaceExisting
   *           if true then replace any existing target object
   *           unless it is a non-empty directory in which case
   *           an IOException is thrown.
   *           False by default
   *  @return
   *           the path to the new copy
   *  @throws IOException
   *           if the copy could not be satisfied because the target could
   *           not be written to or if this path cannot be copied
   *  @throws SecurityEx
   */
  def copyTo[P <: Path](target: P,
             createParents : Boolean=true,
             copyAttributes : Boolean=true,
             replaceExisting : Boolean=false,
             followLinks: Boolean=true,
             depth:Int = Int.MaxValue): P = {

    val linkOptions = if (!followLinks) Seq(LinkOptions.NoFollowLinks) else Seq.empty
    if (exists(linkOptions) && target.exists(linkOptions) && isSame(target)) return target
  
    if(!canRead) throw new SecurityException("Source path: "+path+" does not permit read, therefore copying is denied")

      target.createContainingDir(createParents)

      if(target.parent.exists{p => p.exists && !p.canWrite}) {
        throw new SecurityException("Containing directory of target path: "+target+" does not permit write, therefore copying is denied")
      }

      if (replaceExisting && !target.canWrite) throw new SecurityException("target path: "+target+" does not permit writing, therefore replacing the file is not permitted")
      
    if (isDirectory) {
      if (target.exists && !replaceExisting) fail("target exists but replaceExisting is false")
      
      target.createDirectory(createParents, false)
      if(depth > 0) {
        descendants(depth=depth, linkOptions = linkOptions).foreach{p =>
          val relativePath = self.relativize(p)
          val to = target / relativePath
          p.copyTo(to, depth=0, followLinks=followLinks, replaceExisting=replaceExisting)
        }
      }
    } else {
      var copyOptions = collection.mutable.Buffer[CopyOption]()
      if (copyAttributes) copyOptions += StandardCopyOption.copyAttributes 
      if (replaceExisting) copyOptions += StandardCopyOption.replaceExisting 
      if (followLinks) copyOptions ++= linkOptions
      
      JFiles.copy(jpath, target.jpath, copyOptions:_*)
    }

    target
  }

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
   *  @param replace
   *           if true then replace any existing target object
   *           unless it is a non-empty directory in which case
   *           an IOException is thrown.
   *           False by default
   *  @param createParents
   *           If the parent of target does not exist it will be 
   *           created if createParents is true
   *           True by default
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
  def moveTo[P <: Path](target: P, replace:Boolean=false,
		  	 createParents:Boolean = true,
             atomicMove:Boolean=false) : P = {

    if (root.exists(_ == this)) {
      throw new IOException("cannot move a root path")
    }

    if (target.exists && replace) {
      target match {
        case _ if target.isDirectory && target.children().nonEmpty => fail("can not replace a non-empty directory: " + target)
        case _ => target.delete()
      }
    }

    import PathMatcher._

    implicit val linkOptions = Seq(LinkOptions.NoFollowLinks)
    if (nonExistent) {
      fail("attempted to move " + path + " but it does not exist")
    }
    if (target.normalize == normalize) {
      return target
    }
    if (!replace && target.exists) {
      fail(target + " exists but replace parameter is false")
    }
    if (isFile && target.isDirectory) {
      fail("cannot overwrite a directory with a file")
    }
    if (isDirectory && target.isFile) {
      fail("cannot overwrite a file with a directory")
    }

    if (createParents) {
      target.toAbsolute.parent.collect {
        case parent if parent.nonExistent => parent.createDirectory()
      }
    }
    if (this.isDirectory && (target.fileSystem != fileSystem || jpath.toFile == null || target.jpath == null)) {
      forceMoveDir(target, atomicMove)
    } else if (isFile && (target.nonExistent || target.isFile)) {
      doMove(target, atomicMove)
    } else if (isDirectory) {
      if (!jpath.toFile.renameTo(target.jpath.toFile)) {
        forceMoveDir(target, atomicMove)
      }
    } else {
      throw new Error("not yet handled " + target + "," + this)
    }
    target
  }

  private def forceMoveDir(target:Path, atomicMove:Boolean) = {
    if (atomicMove) {
      fail("Cannot perform an atomic move from %s to %s".format(this, target))
    }
    target.createDirectory()
    children() foreach { path =>
      path moveTo (target resolve (self.relativize(path).segments:_*) )
    }
    delete()
  }
  /**
   * Called to move the current path to another location <strong>on the same filesystem</strong>
   */
  private def doMove(target:Path, atomicMove : Boolean) : Unit = {
    val copyOptions = if (atomicMove) Seq(java.nio.file.StandardCopyOption.ATOMIC_MOVE) else Nil
    JFiles.move(jpath, target.jpath, copyOptions:_*)
  }
  
  override def toString() = "%s(%s)".format(fileSystem.name, path)
  override def equals(other: Any) = other match {
    case x : Path =>
      val eqFS = fileSystem == x.fileSystem
      val eqSeg = jpath == x.jpath
      eqFS && eqSeg
    case _        => false
  }
  override def hashCode() = (7 + jpath.hashCode()) * 37 + fileSystem.hashCode()

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
   *          Default is PathMatcher.All
   * @return
   *          A managed resource managing a PathSet.
   *
   * @see Path#walkTree
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
   def children[U >: Path, F](filter:F = PathMatcher.All, options:Seq[LinkOption]=Nil)(implicit factory:PathMatcherFactory[F]) : PathSet[Path] =
          descendants(filter, depth=1, options)

   def *[F](filter: F)(implicit factory:PathMatcherFactory[F]): PathSet[Path] = {
     children(factory(filter))
   }

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
   *          Default is PathMatcher.All
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
  def descendants[U >: Path, F](filter:F = PathMatcher.All,
           depth:Int = -1, linkOptions:Seq[LinkOption]=Nil)(implicit factory:PathMatcherFactory[F]): PathSet[Path] = {
    if (!isDirectory) throw new NotDirectoryException(this + " is not a directory so descendants can not be called on it")

    new BasicPathSet[Path](this, factory(filter), depth, false, { (p:PathMatcher[Path], path:Path) =>
      // TODO Native filters
      new CloseableIterator[Path] {
        val baseFile = {
          val nonSymlink = path.readSymbolicLink getOrElse path
          if (nonSymlink.exists) {
            nonSymlink
          } else {
            path.parent.map(_ / nonSymlink) getOrElse {throw new IOException("Unable to resolve symlink: "+path+".  Resolved to: "+nonSymlink)}
          }
        }
        val stream = JFiles.newDirectoryStream(baseFile.jpath.toAbsolutePath())

        val iter = stream.iterator
        def doClose = try {stream.close; Nil} catch {case e:Throwable => List(e)}
        def hasNext = iter.hasNext
        def next = {
          new Path(iter.next, fileSystem)
        }
      }
    })
  }
  

  override def **[F](filter: F)(implicit factory:PathMatcherFactory[F]): PathSet[Path] = {
    descendants(factory(filter))
  }
  override def *** : PathSet[Path] = descendants()

  override def +++[U >: Path](includes: PathFinder[U]): PathSet[U] =  new IterablePathSet[U](iterator) +++ includes
  override def ---[U >: Path](excludes: PathFinder[U]): PathSet[Path] = new IterablePathSet[Path](iterator) --- excludes

  override def iterator : CloseableIterator[Path] = CloseableIterator(Iterator(this))


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
  override def copyDataTo(output: Output): Unit = channel(StandardOpenOption.Read).copyDataTo(output)
  override def doCopyFrom(input:Input): Unit = channel(StandardOpenOption.WriteTruncate:_*).doCopyFrom(input)
  def context = fileSystem.context

}
