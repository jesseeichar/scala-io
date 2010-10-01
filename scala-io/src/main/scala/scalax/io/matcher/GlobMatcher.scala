package scalax.io.matcher

import util.parsing.combinator.RegexParsers
  import java.util.regex.Pattern
import scalax.io.{Path, PathMatcher, FileSystem}

/**
 * 
 * User: jeichar
 * Date: Sep 24, 2010
 * Time: 9:39:25 PM
 */
final class GlobMatcher(query:String) extends PathMatcher {
  class GlobParser(fileSystem:FileSystem) extends RegexParsers {
    import Pattern.quote
    val doubleStar: Parser[Any] = "**"
    val star: Parser[Any] = "*"
    val question: Parser[Any] = "?"
    val value: Parser[Any] = ("[^*?/"+fileSystem.separator+"]").r
    val segment: Parser[Any] = rep(doubleStar | star | question | value)
    val separator: Parser[Any] = ("/|"+quote(fileSystem.separator)).r
    val root: Parser[Any] = fileSystem.roots map {r => quote(r.path)} mkString ("|") r
    val path: Parser[Any] = opt(root) ~ repsep(segment,separator)
  }

  def apply(path: Path): Boolean = {
    new GlobParser(path.fileSystem)
    val parsedQuery = ""
    new RegexMatcher(parsedQuery)(path)
  }
}