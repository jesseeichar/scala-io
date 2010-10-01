package scalax.io.matcher

import scalax.io.{Path, PathMatcher}
import util.matching.Regex
import java.util.regex.Pattern

/**
 * 
 * User: jeichar
 * Date: Oct 1, 2010
 * Time: 8:29:21 AM
 */
class FunctionMatcher(f:Path => Boolean) extends PathMatcher {
  def apply(path: Path) = f(path)
}
final class RegexMatcher(pattern:Pattern) extends PathMatcher {
  def this(regex:Regex) = this(regex.pattern)
  def this(query:String) = this(query.r.pattern)
  def apply(path: Path): Boolean = pattern.matcher(path.path).matches
}