/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

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
}

object PathMatcher {
  val ALL = new PathMatcher{
    def apply(path: Path) = true
  }
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
