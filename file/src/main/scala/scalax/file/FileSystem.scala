/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file

import defaultfs.DefaultFileSystem
import java.net.URLStreamHandler
import PathMatcher.{ GlobPathMatcher, RegexPathMatcher }
import util.Random.nextInt
import java.io.{ IOException, File => JFile }
import scalax.io.ResourceContext
import java.util.regex.Pattern

/**
 * Factory object for obtaining filesystem objects
 *
 * @todo add factory methods for creating non-default file systems
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
object FileSystem {
  /**
   *  The default filesystem
   *  <p> In a typical system this is the main system drive
   *  and corresponds to the file system that is referenced by
   *  java.file.File objects</p>
   */
  val default: DefaultFileSystem = new scalax.file.defaultfs.DefaultFileSystem()

}

/**
 * Provides an interface to a file system and is a factory for other objects
 * for accessing files and directories.  Also is used for obtaining metadata
 * about the filesystem.
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
abstract class FileSystem {
  type PathType <: Path
  lazy val presentWorkingDirectory = apply(".").toAbsolute
  protected val legalChars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ List('_', '-', '+', '.') toList
  def randomPrefix = {
    val seg = 1 to (nextInt(5) + 2) map { _ => legalChars(nextInt(legalChars.size)) }
    val lastChar = legalChars(nextInt(legalChars.size - 1))
    seg :+ lastChar mkString ""
  }
  /**
   * Get the Resource context associated with this FileSystem instance.
   *
   * @note as FileSystems are immutable objects a given Resource instance will always be associated with
   * the same ResourceContext
   *
   * @return the associated ResourceContext
   */
  def context:ResourceContext
  /**
   * Create a new FileSystem instance that is configured with the new ResourceContext
   *
   * @param newContext a new ResourceContext
   *
   * @return a new instance configured with the new context
   */
  def updateContext(newContext:ResourceContext):FileSystem
  /**
   * Update the current ResourceContext and return a new FileSystem instance with the updated context
   *
   * @param f A function for transforming the current context to a new context with new values.
   *
   * @return a new instance configured with the new context
   */
  def updateContext(f:ResourceContext => ResourceContext):FileSystem = updateContext(f(context))

  /** A name identifying the filesystem */
  def name: String
  /** The path segment separator string for the filesystem */
  def separator: String
  /**
   * Create a path object for the filesystem
   */
  def fromString(path: String): PathType = {
    val segments = if (separator.size == 1) path.split(separator(0)) else path.split(Pattern.quote(separator))
    doCreateFromSeq((if(path startsWith separator) List(this.separator) else Nil) ++ segments)
  }
  protected def doCreateFromSeq(segments: Seq[String]): PathType
  /**
   * Create a path object for the filesystem from the path segments
   */
  def fromSeq(segments: Seq[String]): PathType = {
    val nonEmpty = segments.filterNot { _.isEmpty }
    val head = nonEmpty.headOption getOrElse "."
    if (head == separator) {
      val parts = nonEmpty.drop(separator.length)
      parts.foreach(checkSegmentForSeparators)
    } else {
      nonEmpty.foreach(checkSegmentForSeparators)
    }
    doCreateFromSeq(nonEmpty)
  }

  def apply(segments: String*): PathType = fromSeq(segments)
  def apply(pathRepresentation: String, separator:Char): PathType = {
    val segments = (if(pathRepresentation.charAt(0) == separator) List(this.separator) else Nil) ++ pathRepresentation.split(separator)
    fromSeq(segments)
  }
  /**
   * Returns the list of roots for this filesystem
   */
  def roots: Set[PathType]

  /**
   * Provides fast access to the fileSystem roots that were present
   * when the fileSystem was created
   */
  private[scalax] lazy val cachedRoots: Set[PathType] = roots
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
   *          Default is glob
   *
   * @return
   *          a function that matches paths against the matching specification in syntax and Pattern
   *
   * @see Path#contents
   */
  def matcher(pattern: String, syntax: String = PathMatcher.StandardSyntax.GLOB): PathMatcher[Path] = {
    syntax match {
      case PathMatcher.StandardSyntax.GLOB => GlobPathMatcher(pattern)
      case PathMatcher.StandardSyntax.REGEX => RegexPathMatcher(pattern)
      case _ => throw new IOException(syntax + " is not a recognized syntax for the " + name + " filesystem")
    }
  }
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
   * @throws java.lang.UnsupportedOperationException
   *          If the filesystem does not support temporary files
   */
  def createTempFile(prefix: String = randomPrefix,
    suffix: String = null,
    dir: String = null,
    deleteOnExit: Boolean = true /*attributes:List[FileAttributes] TODO */ ): PathType
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
   *
   * @throws java.lang.UnsupportedOperationException
   *          If the filesystem does not support temporary files
   */
  def createTempDirectory(prefix: String = randomPrefix,
    suffix: String = null,
    dir: String = null,
    deleteOnExit: Boolean = true /*attributes:List[FileAttributes] TODO */ ): PathType

  /**
   * Returns a URLStreamHandler if the protocol in the URI is not supported by default JAVA.
   * This handler is used to construct URLs for the Paths as well as by scalax.file.PathURLStreamHandlerFactory
   * The default behavoir is to return None this assumes that the default protocol handlers can handle the protocol
   */
  def urlStreamHandler: Option[URLStreamHandler] = None

  /**
   * Checks if the separator or a "Common" separator is in the segment.
   *
   * If the separator is found the an IllegalArgumentException is thrown.
   * If a common separator is found (/ or \) then a warning is logged and the stack trace is logged if fine
   * is enabled for the filesystem's logger.
   *
   * @throws IllegalArgumentException If the separator is found the an IllegalArgumentException is thrown
   */
  def checkSegmentForSeparators(segment: String): Unit = {
    val CommonSeparators = Set('/', '\\')
    sealed trait SeparatorContainment
    case class Separator(sep: String) extends SeparatorContainment
    case class CommonSeparator(sep: Char) extends SeparatorContainment
    case object NoSeparator extends SeparatorContainment

    def findSingleCharSep(seps: Set[Char]): Option[Char] = segment.find(sep => seps contains sep)
    val result = {
      if (separator.size == 1) {
        findSingleCharSep(CommonSeparators + separator(0)).map {
          case sep if separator(0) == sep => Separator(separator)
          case sep => CommonSeparator(sep)
        } getOrElse NoSeparator
      } else {
        if (segment contains separator) Separator(separator)
        else findSingleCharSep(CommonSeparators).map(CommonSeparator.apply) getOrElse NoSeparator
      }
    }

    result match {
      case Separator(sep) =>
        val msg = "%s is not permitted as a path 'segment' for this filesystem.  Segment in question: %s.  " +
          "\nIf you want to create a Path from a system dependent string then use fromString.  " +
          "If you want to create a child path use resolve instead of / to create the child path.  " +
          "It should be noted that the string after '/' must be a single segment but resolve accepts " +
          "full strings. Examples: \n\tPath.fromString(\"c:\\a\\b\")\n\tpath / (\"a/b/c\", '/')\n\tpath resolve \"a\\b\\c\""
        throw new IllegalArgumentException(msg.format(sep , segment))
      case CommonSeparator(sep) => {
         logger.warning(sep + " should not be used as a character in a path segment because it is a commonly used path separator on many filesystems.  Segment in question: "+segment)
         if(logger.isLoggable(java.util.logging.Level.FINE)) {
           val stacktrace = new Exception("Not real exception, just method for obtaining stacktrace").getStackTraceString
           logger.fine("Location where path was created was: ===========\n"+stacktrace+"\n===============================")
         }
      }
      case _ => ()
    }
  }

  protected lazy val logger = java.util.logging.Logger.getLogger(getClass.getPackage().getName())

}
