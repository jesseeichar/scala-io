import java.nio.charset.Charset
import java.io.File
import sbt.IO
import util.matching._
import xml.{XML, NodeSeq, Elem, Node}


class Example(project:String, path:File) {
  private val text = IO.read(path, Charset.forName("UTF-8"))
  val base = path.getName.take(path.getName.size - ".scala".size)
  val name = base.mkString.split("-").map{_.capitalize} mkString " "
  
  private def cleanSummary(summary:String) = summary.lines.map { _.trim.replaceAll("\\*\\s*","")}.mkString(" ").replaceAll("\\s\\s"," ").replace(". ",".  ")
  private def shortSummary(summary:String):String = summary.takeWhile(_ != '.') match {
    case s if s.size > 100 => s.take(100).mkString+"..."
    case s => s.mkString
  }
  lazy val rawDesc = {
    val regex:Regex = """(?s)/\*\*(.+?)\*/\s*object""".r
    val firstMatch = regex.findFirstMatchIn(text)
    firstMatch match {
      case Some(matcher) if !matcher.before.toString.contains("object") =>
        cleanSummary(matcher.group(1))
      case _ => ""
    }
  }
  lazy val summary = rawDesc.takeWhile{_ != '.'} mkString
  lazy val description:Node = parseXML(base,if(rawDesc == summary) "" else rawDesc.dropWhile{_ != '.'}.drop(1).mkString)
  lazy val shortSummaryText:String = shortSummary(summary)
  lazy val keyword = Keyword(project, name.toLowerCase,name,1,name,"category",Set(name))
  lazy val page = Page(keyword,description,shortSummaryText,"")
  
  lazy val pages:Seq[Page] = {
    
    val objText = text.drop(text.indexOf("object"))
    val regex:Regex = """(?s)(/\*\*(.+?)\*/\s*)?def\s+(\S+)\s*=?\s*\{""".r
    regex.findAllIn(objText).matchData.toList map {m =>
      val rawName = m.group(3)
      val name = rawName.flatMap {
        case c if c.isUpperCase => " "+c
        case c => c.toString
      }.mkString.capitalize.trim

      val summary = if(m.group(2) == null) "" else cleanSummary(m.group(2))
      val summaryXml = parseXML(base+"#"+name, summary)

      val (_,code:String) = ((1,"") /: m.after.toString) {
        case ((0,text),_) => (0,text)
        case ((1,text),'}') => (0,text)
        case ((depth,text),'}') => (depth-1,text+'}')
        case ((depth,text),'{') => (depth+1,text+'{')
        case ((depth,text),c) => (depth,text+c)
      }
      val trimmedCode = {
        def ws(l:String) = "^(\\s*)".r.findFirstMatchIn(l).get.group(1).size
        var indent = 0
        val lines = code.lines.foldLeft ("") {
          case ("",l) =>
            indent = ws(l)
            l.substring(indent) + "\n"
          case (acc,l) =>
            acc + l.substring(Math.min(ws(l),indent)) + "\n"
        }
        lines mkString
      }
      val keyword = Keyword(project,name.toLowerCase,name,2,name,"example",Keyword.split(summary))
      Page(keyword,summaryXml,shortSummary(summary),code)
    }
  }
  
  private def parseXML(name:String,xml:String)= {
      try {
        XML.loadString("<span>"+xml+"</span>")
      } catch {
        case _ => throw new RuntimeException(name+" has invalid XML found in "+ xml)
      }
  }
}

case class Page(keyword:Keyword, fullSummaryXml:Node, shortSummary:String, code:String) {
  
}