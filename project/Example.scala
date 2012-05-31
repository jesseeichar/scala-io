import java.nio.charset.Charset
import java.io.File
import sbt.IO
import util.matching._
import xml.{XML, NodeSeq, Elem, Node}


class Example(project:String, path:File) {
  parentPage =>
  private val text = IO.read(path, Charset.forName("UTF-8"))
  val base = path.getName.take(path.getName.size - ".scala".size)
  val name = base.mkString.split("-").map{_.capitalize} mkString " "

  private def cleanSummary(summary:String) = summary.lines.map { _.trim.replaceAll("\\*\\s*","")}.mkString(" ").replaceAll("\\s\\s"," ").replace(". ",".  ")
  private def shortSummary(summary:String):String = summary.takeWhile(_ != '.').mkString.replaceAll("\"","'")
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
  lazy val description:Node = parseXML(base,rawDesc)
  lazy val shortSummaryText:String = shortSummary(summary)
  lazy val keyword = Keyword(project, name.toLowerCase,name,1,name,"category",project,shortSummaryText, Set(name))
  lazy val page = Page(keyword,description,"",pages)

  lazy val pages:Seq[Page] = {

    val objText = text.drop(text.indexOf("object"))
    val regex:Regex = """(?s)(/\*\*(.+?)\*/\s*)?def\s+(\S+)\s*=?\s*\{""".r
    regex.findAllIn(objText).matchData.toList map {m =>
      val rawName = m.group(3)
      val name = rawName.flatMap {
        case c if c.isUpper => " "+c
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
      val keyword = Keyword(project,name.toLowerCase.replaceAll("\\s","_"),name,2,name,"example",parentPage.keyword.id,shortSummary(summary),Keyword.split(code))
      Page(keyword,summaryXml,code)
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

case class Page(keyword:Keyword, fullSummaryXml:Node, code:String, children:Seq[Page]=Nil) {
  def html = <span>
    <div class="example_summary"><span>{fullSummaryXml}</span></div>
    {if(code.trim().nonEmpty) <div class="example_code"><pre class="brush: scala">{code}</pre></div> else ""}
    {
      if(children.isEmpty) {
        <span/>
      } else {
        <ul id="content-list" ng:class="sectionId">
          <li ng:repeat="example in examples" ng:class="getClass(example)">
            <a href="{{getUrl(example)}}" ng:class="selectedPartial(example)"
               tabindex="3"> <strong>{"{{example.shortName}}"}</strong> - <em>{"{{example.shortSummary}}"}</em></a>
          </li>
        </ul>
      }
    }
  </span>
}
