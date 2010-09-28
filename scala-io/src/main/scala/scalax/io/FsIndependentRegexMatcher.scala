package scalax.io

/**
 * 
 * User: jeichar
 * Date: Sep 24, 2010
 * Time: 9:39:25 PM
 */

final class FsIndependentRegexMatcher(query:String) extends PathMatcher {
  val pattern = query.r.pattern
  def apply(path: Path): Boolean = pattern.matcher(path.path).matches
}