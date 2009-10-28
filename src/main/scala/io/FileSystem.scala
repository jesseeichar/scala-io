/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{File=>JFile}

/**
 * Factory object for obtaining filesystem objects
 */
object FileSystem {
  /**
   *  The default filesystem
   *  <p> In a typical system this is the main system drive
   *  and corresponds to the file system that is referenced by
   *  java.io.File objects</p>
   */
  val defaultFileSystem: FileSystem = new DefaultFileSystem()
}

/**
 * Provides an interface to a file system and is a factory for other objects
 * for accessing files and directories.  Also is used for obtaining metadata
 * about the filesystem.
 */
abstract class FileSystem {
  /** The path segment separator string for the filesystem */
  def separator: String
  /**
   * Create a path object for the filesystem
   */
  def apply(path: String): Path
  /**
   * Returns the list of roots for this filesystem
   */
  def roots: List[Path]
  /**
   * Creates a function that returns true if parameter matches the
   * pattern used to create the function.
   * <p>
   * If the syntax is glob then the following patterns are accepted:
   * <ul>
   * <li>The * character matches zero or more characters of a name component without 
   *     crossing directory boundaries.</li>
   * <li><The ** characters matches zero or more characters crossing directory boundaries.</li>
   * <li>The ? character matches exactly one character of a name component.</li>
   * <li>The backslash character (\) is used to escape characters that would otherwise be 
   *     interpreted as special characters. The expression \\ matches a single backslash 
   *     and "\{" matches a left brace for example.</li>
   * </ul>
   * Currently unsupported slated to be supported shortly are:
   * <ul>
   * <li>The [ ] characters are a bracket expression that match a single character of a 
   *     name component out of a set of characters. For example, [abc] matches "a", "b", or "c". 
   *     The hyphen (-) may be used to specify a range so [a-z] specifies a range that matches 
   *     from "a" to "z" (inclusive). These forms can be mixed so [abce-g] matches "a", "b", "c", 
   *     "e", "f" or "g". If the character after the [ is a ! then it is used for negation so 
   *     [!a-c] matches any character except "a", "b", or "c".</li>
   * <li>Within a bracket expression the *, ? and \ characters match themselves. The (-) character 
   *     matches itself if it is the first character within the brackets, or the first character
   *     after the ! if negating.</li>
   * <li>The { } characters are a group of subpatterns, where the group matches if any subpattern 
   *     in the group matches. The "," character is used to separate the subpatterns. Groups 
   *     cannot be nested.</li>
   * <li>All other characters match themselves in an implementation dependent manner. This 
   *     includes characters representing any name-separators.</li>
   * </ul>
   * The matching of root components is highly implementation-dependent and is not specified.
   * <p> 
   * If the syntax is regex then the pattern component is a pattern as defined by the 
   * {@link java.util.regex.Pattern} class
   * </p>
   * <p>In both cases the matching is case sensitive</p>
   *
   * @param pattern 
   *          the pattern of the match
   * @param syntax 
   *          the identifier of the syntax that will be used to interpret the pattern
   *
   * @return 
   *          a function that matches paths agains the matching specification in syntax and Pattern
   *
   * @see Path#contents 
   */
  def matcher(pattern:String, syntax:String = "glob"): PathMatcher
  /**
   * Creates an empty file in the provided directory with the provided prefix and suffixes, 
   * if the filesystem supports it.  If not then a UnsupportedOperationException is thrown.
   * <p>
   * The file will not replace an existing file and it is guaranteed to be unique and
   * not previously used by another process at time of creation.
   * </p>
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
   * @throws java.lang.UnsupportedOperationException
   *          If the filesystem does not support temporary files
   */
  def makeTempFile(prefix: String = Path.randomPrefix, 
                   suffix: String = null, 
                   dir: Path = null,
                   deleteOnExit : Boolean = true
                   /*attributes:List[FileAttributes] TODO */ ) : Path 
  /**
   * Creates an empty directory in the provided directory with the provided prefix and suffixes, if the filesystem
   * supports it.  If not then a UnsupportedOperationException is thrown.
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
   *
   * @throws java.lang.UnsupportedOperationException
   *          If the filesystem does not support temporary files
   */
  def makeTempDirectory(prefix: String = Path.randomPrefix,
                        suffix: String = null, 
                        dir: Path = null,
                        deleteOnExit : Boolean = true
                        /*attributes:List[FileAttributes] TODO */) : Path

}

private[io] class DefaultFileSystem extends FileSystem {
  def separator: String = JFile.separator
  def apply(path: String): DefaultPath = apply (new JFile (path))
  def apply(path: JFile): DefaultPath = new DefaultPath (path, this)
  def roots = JFile.listRoots().toList map {f=> apply (f.getPath)}
  def makeTempFile(prefix: String = Path.randomPrefix, 
                   suffix: String = null, 
                   dir: Path = null,
                   deleteOnExit : Boolean = true
                   /*attributes:List[FileAttributes] TODO */ ) : Path = {
    assert(dir.fileSystem == this, "Specified directory must be be in the same filesystem as the filesystem creating the temporary file")
    val path = apply(JFile.createTempFile(prefix, suffix, dir.asInstanceOf[DefaultPath].jfile).getPath)
    if(deleteOnExit) path.jfile.deleteOnExit
    path
  }

  def makeTempDirectory(prefix: String = Path.randomPrefix,
                        suffix: String = null, 
                        dir: Path = null,
                        deleteOnExit : Boolean = true
                        /*attributes:List[FileAttributes] TODO */) : Path = {
    assert(dir.fileSystem == this, "Specified directory must be be in the same filesystem as the filesystem creating the temporary directory")
    val path = Path.makeTempFile(prefix, suffix, dir, false)
    path.delete()
    path.createDirectory()
    if(deleteOnExit) {
      Runtime.getRuntime.addShutdownHook(new Thread{override def run:Unit = path.deleteRecursively(true) })
    }
    path
  }
}
