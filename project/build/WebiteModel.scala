import sbt.{ParentProject, Logger, Path}
import xml.Node
import xsbt.FileUtilities._

object Printer {
  def printPage(to:Path,html:Node,log:Logger) = {
      log.info("Writing "+to.projectRelativePath)
      val htmlWithDocType: String =
        """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
         |"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
         |""".stripMargin+html
      write(to.asFile, htmlWithDocType, utf8)
    }
}


case class Example(htmlName:String, name:String, summary:Node, uberSummary:String, code:String) {
  override def toString = "Example("+name+", "+uberSummary+"\n"+code.substring(0,10)+"...)"
  def html = {
    <div class="example">
      <a name={htmlName}><h3>{name}</h3></a>
      <div class="example_summary">{summary}</div>
      <div class="example_code">
        <pre class='brush: scala'>{code.replace("<","&lt;")}</pre>
      </div>
    </div>
  }
}

class WebsiteModel(val rootProject:ScalaIOProject, val projectSites:List[ProjectSite],val outputDir:Path,log:Logger) {
  val self = this;
  def buildSite = {

    Printer.printPage(outputDir / "index.html",indexHtml,log)
    Printer.printPage(outputDir / "roadmap.html",Roadmap.html(this),log)
    Printer.printPage(outputDir / "getting-started.html",GettingStarted.html(this),log)
    projectSites.foreach {
      project=>
        project.buildSite(self);
    }
  }

  def indexHtml = {
    val content =
      <div class="explanation">
        <p>
        The Scala IO umbrella project consists of a few sub projects for different aspects and
        extensions of IO.  The core project deals with raw input and output, primarily through
        streams and channels (at least the Java implementation).
        </p><p>
        <strong>Warning #1: </strong>The current API is a very early version and is likely to change.
        That said barring a massive outcry the core components should remain fairly consistent. Both the core and file projects will be changing but file is even more likely to change.
        Especially with regard to FileAttributes and the API for implementing custom filesystems.
        </p><p>
        <strong>Warning #2: </strong>I have spend no time on optimization so there are a large number of
        known inefficiencies.  Patches and suggestions are welcome but please keep flaming to a minimum.
        </p><p>
        If you have any suggestions please open a ticket at
        <a href="https://github.com/jesseeichar/scala-io/issues">https://github.com/scala-incubator/scala-io/issues</a>

        I will monitor the tickets and reply on the tickets.
        </p>
      </div>


    Template(false, "Scala IO API Documentation")(
        <link href={"css/samples.css"} rel="stylesheet" type="text/css" ></link>)(
        <h1>Scala IO API Documentation</h1>)(
        content)(Template.rootNavbar(Link.Overview,projectSites))
  }

}
