package scalax.io
  
  import util.parsing.combinator.RegexParsers
  import java.util.regex.Pattern

/**
 * 
 * User: jeichar
 * Date: Sep 24, 2010
 * Time: 9:39:25 PM
 */

final class FsIndependentGlobMatcher(query:String,fileSystem:FileSystem) extends PathMatcher {
  class GlobParser extends RegexParsers {
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
  val parsedQuery = ""
  val regex = new FsIndependentRegexMatcher(parsedQuery)

  def apply(v1: Path): Boolean = regex(v1)
}