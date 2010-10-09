package scalax.io

import util.matching.Regex
import java.util.regex.Pattern


/**
 * Object containing several objects that make matching
 * certain types of Paths much easier.
 * <p>
 * Example:
 * <pre><code>
 * val Apple = path.matcher (pattern="[aA]pple", syntax="glob")
 * val ReadWrite = new AccessMatcher (Read, Write)
 * path match {
 *   case File(f) => println("A file was found")
 *   case Directory(d) => println("A directory was found")
 *   case NonExistent(e) => println("Path does not Exist")
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
  /** matches all paths*/
  object All extends PathMatcher {
    def apply(path:Path) = true
  }
  /** matches a path if it is a file (and exists)*/
  object IsFile extends PathMatcher {
    def apply(path:Path) = path.isFile
  }
  /** matches a path if it is a Directory (and exists)*/
  object IsDirectory extends PathMatcher {
    def apply(path:Path) = path.isDirectory
  }
  /** matches a path if it is Exists */
  object Exists extends PathMatcher {
    def apply(path:Path) = path.exists
  }
  /** matches a path if it does not Exist */
  object NonExistent extends PathMatcher {
    def apply(path:Path) = path.nonExistent
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
  final class AccessMatcher (accessModes: Path.AccessModes.AccessMode*) extends PathMatcher {
    val accessModeSet = Set(accessModes:_*)
    def apply(path:Path) = accessModeSet.intersect(path.access).size == accessModeSet.size
  }
  object AccessMatcher {
    def apply(accessModes: Path.AccessModes.AccessMode*) = new AccessMatcher(accessModes:_*)
  }

  class FunctionMatcher(f:Path => Boolean) extends PathMatcher {
    def apply(path: Path) = f(path)
  }
  final class NameIs(name:String) extends FunctionMatcher(_.name == name)

  final class RegexPathMatcher(pattern:Pattern) extends PathMatcher {
    def this(regex:Regex) = this(regex.pattern)
    def this(query:String) = this(query.r.pattern)
    def apply(path: Path): Boolean = pattern.matcher(path.path).matches
  }

  final class RegexNameMatcher(pattern:Pattern) extends PathMatcher {
    def this(regex:Regex) = this(regex.pattern)
    def this(query:String) = this(query.r.pattern)
    def apply(path: Path): Boolean = pattern.matcher(path.name).matches
  }
  final class GlobPathMatcher(query:String) extends PathMatcher {
    def apply(path: Path): Boolean = {

      val parsedQuery = new GlobParser(path.fileSystem)(query)
      new RegexPathMatcher(parsedQuery)(path)
    }
  }

  final class GlobNameMatcher(query:String) extends PathMatcher {
    def apply(path: Path): Boolean = {

      val parsedQuery = new GlobParser(path.fileSystem)(query)
      new RegexNameMatcher(parsedQuery)(path)
    }
  }
}