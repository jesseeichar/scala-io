
import util.parsing.combinator.RegexParsers
import java.util.regex.Pattern

val sep = "/"
val roots = List("/")
object GlobParser extends RegexParsers {
	import Pattern.quote
  val doubleStar: Parser[Any] = "**"
  val star: Parser[Any] = "*"
  val question: Parser[Any] = "?"
  val escape: Parser[Any] = """\\.?""".r
  val choice: Parser[Any] = '{' ~> repsep(rep1(escape | "[^{}}]".r) ^^ {case l => l mkString},',') <~ '}'
  val group: Parser[Any] = '[' ~> repsep(rep1(escape | """[^\[\]]""".r) ^^ {case l => l mkString},',') <~ ']'
  val value: Parser[Any] = """[^*?/\\\{\}\[\]""" + sep + "]+" r
  val segment: Parser[Any] = rep(group | choice | escape | value | star | question) ||| doubleStar
  val separator: Parser[Any] ="/|" + quote(sep) r
  val root: Parser[Any] = roots map {r => quote(r)} mkString ("|") r
  val path: Parser[Any] = opt(root) ~ repsep(segment,separator)
}

println(GlobParser.parseAll(GlobParser.segment,"h\\?"))
println(GlobParser.parseAll(GlobParser.group,"[a-z]"))
println(GlobParser.parseAll(GlobParser.choice,"{h,he,l, escape?\\}}"))
println(GlobParser.parseAll(GlobParser.path,"hi/o**k"))
println(GlobParser.parseAll(GlobParser.path,"""hi/o\?"""))
println(GlobParser.parseAll(GlobParser.path,"""hi/o\/hi/"""))
println(GlobParser.parseAll(GlobParser.path,"""hi/**/[a-b]*.{scala,java}"""))
