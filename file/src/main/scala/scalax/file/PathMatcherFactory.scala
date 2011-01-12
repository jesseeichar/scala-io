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
trait PathMatcherFactory[-T] extends Function1[T,PathMatcher]

object PathMatcherFactory {
  implicit object FunctionToMatcher extends PathMatcherFactory[Function1[Path,Boolean]] {
    def apply(f: (Path) => Boolean): PathMatcher = f match {
    case m:PathMatcher => m
    case f => new FunctionMatcher(f)
  }
  }
  implicit object GlobToMatcher extends PathMatcherFactory[String] {
    def apply(f: String): PathMatcher = GlobNameMatcher(f)
  }
  implicit object RegexToMatcher extends PathMatcherFactory[Regex] {
    def apply(f: Regex): PathMatcher = RegexNameMatcher(f)
  }
  implicit object PatternToMatcher extends PathMatcherFactory[Pattern] {
    def apply(f: Pattern): PathMatcher = RegexNameMatcher(f)
  }
}
