package scalax.io

import util.parsing.combinator.RegexParsers
import java.util.regex.Pattern
import scalax.io.{Path, PathMatcher, FileSystem}
import scalax.io.matcher.Matching.RegexPathMatcher


class GlobParser(fileSystem:FileSystem) extends RegexParsers {
  import Pattern.quote


  val noSep = "[^"+fileSystem.separator+"]"
  val doubleStar: Parser[Any] = "**" ^^ {case _ => ".*"}
  val star: Parser[Any] = "*" ^^ {case _ => noSep+"*"}
  val question: Parser[Any] = "?" ^^ {case _ => noSep}
  val escape: Parser[String] = """\\.?""".r
  val removeEscape: Parser[String] = escape ^^ {case r => r drop 1}
  val choice: Parser[Any] = '{' ~> repsep(rep1(removeEscape | "[^{}},]".r) ^^ {case l => l mkString},',') <~ '}' ^^ {_.map (quote) mkString ("(","|",")") }
  val group: Parser[Any] = '[' ~> repsep(rep1(escape | """[^\[\]]""".r) ^^ {case l => l mkString},',') <~ ']' ^^ {
    case g =>
      val groups = g.mkString
      val finalGroups = if(groups startsWith "!") '^'+groups.drop(1) else groups
      "["+finalGroups+"]"
  }
  val value: Parser[Any] = ("""[^*?/\\\{\}\[\]""" + fileSystem.separator + "]+").r ^^ {case c => quote(c)}
  val segment: Parser[Any] = rep(group | choice | escape | value | star | question) ^^ {case segment => segment mkString ""} ||| doubleStar
  val separator: Parser[Any] ="/|" + quote(fileSystem.separator) r
  val root: Parser[Any] = fileSystem.roots map {r => quote(r.path)} mkString ("|") r

  val path: Parser[String] = opt(root) ~ repsep(segment,separator) ^^ {
    case Some(root) ~ path  => root + (path mkString fileSystem.separator)
    case None ~ path  => path mkString fileSystem.separator
  }

  def apply(glob:String) : String = parseAll(path,glob) match {
    case Success(r,_) => r
    case Failure(msg,_) => throw new RuntimeException("Failed to parse "+glob+" as a 'glob' pattern: "+msg)
    case Error(msg,_) => throw new RuntimeException("Error parsing "+glob+" as a 'glob' pattern: "+msg)
  }
}
