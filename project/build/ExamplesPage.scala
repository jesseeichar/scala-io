import sbt.{Logger, Path}
import util.matching.Regex
import xml.{NodeSeq, Elem, Node}
import xsbt.FileUtilities._

class ExamplesPage(val path:Path,logger:Logger, project:ProjectSite) {
  val self = this;
  private val text = read(path.asFile)
  val base = path.name.take(path.name.size - ".scala".size)
  val name = base.mkString.split("-").map{_.capitalize} mkString " "
  val htmlName = base+".html"
  private def cleanSummary(summary:String) = summary.replaceAll("(?m)^\\s*\\*\\s*","").replace("\n|\r"," ").trim
  private def uberSummary(summary:String):String = summary.takeWhile(_ != '.') match {
    case s if s.size > 100 => s.take(100).mkString+"..."
    case s => s.mkString
  }
  val summary = {
    val regex:Regex = """(?s)/\*\*(.+?)\*/\s*object""".r
    val firstMatch = regex.findFirstMatchIn(text)
    firstMatch.flatMap{
      case matcher:Regex.Match if !matcher.before.toString.contains("object") =>
        val summaryText = cleanSummary(matcher.group(1))
        Some(summaryText)
      case _ => None
    }
  }
  val uberSummaryText:String = uberSummary(summary getOrElse "")
  val examples = {
    val objText = text.drop(text.indexOf("object"))
    val regex:Regex = """(?s)/\*\*(.+?)\*/\s*def\s+(\S+)\s*=?\s*\{""".r
    regex.findAllIn(objText).matchData.toList map {m =>
      val name = m.group(2).flatMap {
        case c if c.isUpperCase => " "+c
        case c => c.toString
      }.mkString.capitalize.trim

      val summary = cleanSummary(m.group(1))
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
      Example(m.group(2)+".html",name,summary,uberSummary(summary), trimmedCode)
    }
  }

  def pageNavList = {
    <ul id="subnavlist">{for(ex <- examples) yield {
      <li><a title={ex.uberSummary} href={ex.htmlName}>{ex.name}</a></li>
    }}</ul>

  }

  def template(site:WebsiteModel)(exampleHtml:NodeSeq) = {
    <html>
      <head>
        <title>{name}</title>
        <script type="text/javascript" src="../../js/shCore.js"></script>
        <script type="text/javascript" src="../../js/shBrushScala.js"></script>
        <link href="../../css/shCore.css" rel="stylesheet" type="text/css"></link>
        <link href="../../css/shThemeDefault.css" rel="stylesheet" type="text/css" ></link>
        <link href="../../css/samples.css" rel="stylesheet" type="text/css" ></link>
      </head>
      <body>
        <div id="maincontainer">
          <div id="topsection">
            <div class="innertube">
              <h1>{name}</h1>
              {summary.map{s => <p class="summary">{s}</p>} getOrElse ""}
            </div>
          </div>

          <div id="contentwrapper">
            <div id="contentcolumn">
              <div class="innertube">
                {exampleHtml}
              </div>
            </div>
          </div>

          <div id="leftcolumn">
            <div class="innertube">
              {project.navbar(site, self, "../../")}
            </div>
          </div>

          <div id="footer">
            <a href="http://www.dynamicdrive.com/style/">Dynamic Drive CSS Library</a>
          </div>
        </div>

        <script type="text/javascript">
          SyntaxHighlighter.all()
        </script>
      </body>
    </html>
  }
  def html(site:WebsiteModel) = {
    template(site){
      val html:NodeSeq = for(ex <- examples) yield {
        ex.html
      }
      html
    }
  }

}
