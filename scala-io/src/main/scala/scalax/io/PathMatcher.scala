/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.io.PathMatcher.FunctionMatcher
import util.matching.Regex
import java.util.regex.Pattern

/**
 * A function that returns true if the Path parameter
 * matches.
 * <p>
 * PathMatchers are created by {@link FileSystem#matcher(String,String)}
 * and the same method explains how to specify a matcher.
 * Instances of this class can be used in match statements.
 * </p><p>
 * An example usage is:
 * <pre><code>
 * val Code = fileSystem.matcher("*.scala")
 * Path("src").contents (0){case (Code(_),count) => count+1} 
 * </code></pre>
 *
 * The above example counts all the scala files in the src directory.
 *
 * @see FileSystem#matcher(String,String)
 * @see Path#matcher(String,String)
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
abstract class PathMatcher extends Function[Path,Boolean] {
  def unapply(path: Path) = if(apply(path)) Some(path) else None
  
  def || (filter: PathMatcher): PathMatcher = new FunctionMatcher( path => apply(path) || filter(path), this + " || " + filter)
  def && (filter: PathMatcher): PathMatcher = new FunctionMatcher( path => apply(path) && filter(path), this + " && " + filter)
  def -- (filter: PathMatcher): PathMatcher = new FunctionMatcher( path => apply(path) && !filter(path), this + " -- " + filter)
  def unary_- : PathMatcher = new FunctionMatcher( path => !apply(path), "not (" + this +")")
}

object PathMatcher {

  /** matches all paths*/
  object All extends PathMatcher {
    def apply(path:Path) = true
    override def toString = "All Matcher"
  }
  /** matches a path if it is a file (and exists)*/
  object IsFile extends PathMatcher {
    def apply(path:Path) = path.isFile
    override def toString = "IsFile Matcher"
  }
  /** matches a path if it is a Directory (and exists)*/
  object IsDirectory extends PathMatcher {
    def apply(path:Path) = path.isDirectory
    override def toString = "IsDirectory Matcher"
  }
  /** matches a path if it is Exists */
  object Exists extends PathMatcher {
    def apply(path:Path) = path.exists
    override def toString = "Exists Matcher"
  }
  /** matches a path if it does not Exist */
  object NonExistent extends PathMatcher {
    def apply(path:Path) = path.nonExistent
    override def toString = "NonExistent Matcher"
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
    override def toString = "AccessMatcher: "+(accessModeSet mkString ",")
  }
  object AccessMatcher {
    def apply(accessModes: Path.AccessModes.AccessMode*) = new AccessMatcher(accessModes:_*)
  }

  class FunctionMatcher(f:Path => Boolean, name:String = "") extends PathMatcher {
    def apply(path: Path) = f(path)
    override def toString = "FunctionMatcher: "+ (if (name == "") f.toString else name)
  }
  final case class NameIs(name:String) extends FunctionMatcher(_.name == name) {
    override def toString = "NameIs: "+name
  }

  final class RegexPathMatcher(pattern:Pattern) extends PathMatcher {
    def apply(path: Path): Boolean = pattern.matcher(path.path).matches
    override def toString = "RegexPathMatcher: "+pattern
  }
  object RegexPathMatcher {
    def apply(pattern:Pattern) = new RegexPathMatcher(pattern)
    def apply(regex:Regex) = new RegexPathMatcher(regex.pattern)
    def apply(query:String) = new RegexPathMatcher(query.r.pattern)
  }

  final class RegexNameMatcher(pattern:Pattern) extends PathMatcher {
    def apply(path: Path): Boolean = pattern.matcher(path.name).matches
    override def toString = "RegexNameMatcher: "+pattern
  }
  object RegexNameMatcher {
    def apply(pattern:Pattern) = new RegexNameMatcher(pattern)
    def apply(regex:Regex) = new RegexNameMatcher(regex.pattern)
    def apply(query:String) = new RegexNameMatcher(query.r.pattern)
  }

  final class GlobPathMatcher(query:String) extends PathMatcher {
    def apply(path: Path): Boolean = {

      val parsedQuery = new GlobParser(path.fileSystem)(query)
      RegexPathMatcher(parsedQuery)(path)
    }
    override def toString = "GlobPathMatcher: "+query
  }
  object GlobPathMatcher { def apply(query : String) = new GlobPathMatcher(query) }
  final class GlobNameMatcher(query:String) extends PathMatcher {
    def apply(path: Path): Boolean = {

      val parsedQuery = new GlobParser(path.fileSystem)(query)
      RegexNameMatcher(parsedQuery)(path)
    }
    override def toString = "GlobNameMatcher: "+query
  }
  object GlobNameMatcher { def apply(query : String) = new GlobNameMatcher(query) }

  /**
   * Contains the constants for the different
   * PathMatcher syntaxes that are supported by all
   * filesystems
   *
   * @see FileSystem#matcher(String,String) 
   * @see Path#matcher(String,String)
   * 
   * @author  Jesse Eichar
   * @since   1.0
   */
  object StandardSyntax {
    /** Glob matcher sytax */
    final val GLOB = "glob"
    /** Regex matcher sytax */
    final val REGEX = "regex"
  }
}
