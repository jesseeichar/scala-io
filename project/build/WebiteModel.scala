import sbt.{Logger, Path}
import util.matching.Regex
import xsbt.FileUtilities._

class ProjectSite(project:IoProject,logger:Logger) {
  val name = project.name
  val description = "description TODO"
  val path = project.info.projectPath.name
  def pagePath(page:Page) = path+"/"+page.htmlName

  val pages = project.samplesSources.get map {path => new Page(path,logger)}
}

class Page(val path:Path,logger:Logger) {
  private val text = read(path.asFile)
  private val base = path.name.take(path.name.size - ".scala".size)
  val name = base.mkString.split("-").map{_.capitalize} mkString " "
  val htmlName = base+".html"
  private def cleanSummary(summary:String) = summary.replaceAll("^\\s*\\*","").replace("\n|\r"," ")
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
      Example(m.group(2),name,summary,trimmedCode)
    }
  }
}

case class Example(href:String, name:String, summary:String, code:String) {
  override def toString = "Example("+name+", "+summary.substring(0,10)+"..., "+code.substring(0,10)+"...)"
}

class WebsiteModel(projectSites:List[ProjectSite],outputDir:Path,log:Logger) {
  def pageNavList(page:Page) = {
    <ul id="subnavlist">{for(ex <- page.examples) yield {
      <li><a href={"#"+ex.href}>{ex.name}</a></li>
    }}</ul>

  }
  def navbar(currPage:Page) = {
      <ul id="navlist">{for(project <- projectSites) yield {
        <li>{project.name}
          <ul>{for(page <- project.pages) yield {
                if(currPage == page ){
                  <li id="active">
                    <a id="active" href={"../"+project.pagePath(page)}>{page.name}</a>
                    {pageNavList(page)}
                  </li>
                } else {
                    <li><a href={"../"+project.pagePath(page)}>{page.name}</a></li>
                }
            }}</ul>
        </li>
      }}</ul>
  }

  def pageTemplate(page:Page) = {
    <html>
      <head>
        <title>{page.name}</title>
        <script type="text/javascript" src="../js/shCore.js"></script>
        <script type="text/javascript" src="../js/shBrushScala.js"></script>
        <link href="../css/shCore.css" rel="stylesheet" type="text/css"></link>
        <link href="../css/shThemeDefault.css" rel="stylesheet" type="text/css" ></link>
        <link href="../css/samples.css" rel="stylesheet" type="text/css" ></link>
      </head>
      <body>
        <div id="maincontainer">
          <div id="topsection">
            <div class="innertube">
              <h1>{page.name}</h1>
              {page.summary.map{s => <h3>{s}</h3>} getOrElse ""}
            </div>
          </div>

          <div id="contentwrapper">
            <div id="contentcolumn">
              <div class="innertube">
                {for(ex <- page.examples) yield {
                  <div class="example">
                    <a name={ex.href}></a><h2>{ex.name}</h2>
                    <p class="example_summary">{ex.summary}</p>
                    <pre class='brush: scala'>{ex.code.replace("<","&lt;")}</pre>
                  </div>}
                }
              </div>
            </div>
          </div>

          <div id="leftcolumn">
            <div class="innertube">
              {navbar(page)}
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

  def projectTemplate(project:ProjectSite) {

  }

  def printPage(to:Path,page:Page) = {
    log.info("Writing "+page.name+" to "+to.projectRelativePath)
    val html: String =
      """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
       |"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
       |""".stripMargin+pageTemplate(page).toString
    write(to.asFile, html,utf8)
  }

  def buildSite = {
    projectSites.foreach {
      project=>
        val projDir = outputDir / project.path
        projDir.asFile.mkdirs
        project.pages foreach{p => printPage(projDir / p.htmlName,p)}
    }
  }
}
