package scalax.file

import PathMatcher.{GlobNameMatcher, RegexNameMatcher, FunctionMatcher}
import util.matching.Regex
import java.util.regex.Pattern

/**
 *
 * User: jeichar
 * Date: Oct 1, 2010
 * Time: 8:23:16 AM
 */
trait PathMatcherFactory[-T] extends Function1[T,PathMatcher[Path]]

object PathMatcherFactory {
  implicit object FunctionToMatcher extends PathMatcherFactory[Function1[Path,Boolean]] {
    def apply(f: (Path) => Boolean): PathMatcher[Path] = f match {
    case m:PathMatcher[Path] => m
    case f => new FunctionMatcher(f)
  }
  }
  implicit object GlobToMatcher extends PathMatcherFactory[String] {
    def apply(f: String): PathMatcher[Path] = GlobNameMatcher(f)
  }
  implicit object RegexToMatcher extends PathMatcherFactory[Regex] {
    def apply(f: Regex): PathMatcher[Path] = RegexNameMatcher(f)
  }
  implicit object PatternToMatcher extends PathMatcherFactory[Pattern] {
    def apply(f: Pattern): PathMatcher[Path] = RegexNameMatcher(f)
  }
}
