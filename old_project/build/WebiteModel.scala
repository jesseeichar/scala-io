import java.nio.charset.Charset
import sbt.{ExactFilter, ParentProject, Logger, Path}
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

class WebsiteModel(val websiteProj:ScalaIOProject#WebSite, val rootProject:ScalaIOProject, val projectSites:List[ProjectSite],val outputDir:Path,log:Logger) {
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

  def read(name:String) = {
    val file = (websiteProj.sourcePath ** new ExactFilter(name)).getFiles.toStream.head
    sbt.FileUtilities.readString(file,Charset.forName("UTF-8"),log).fold(
      s => throw new Exception(s),
      s => s
    )
  }
  val javaExample = read("JavaIOExample.java")
  val scalaExample = read("ScalaIOExample.scala")

  def indexHtml = {
    val content =
      <span>
      <div class="explanation">
        <p>
        The Scala IO umbrella project consists of a few sub projects for different aspects and
        extensions of IO.  The core project deals with raw input and output, primarily through
        streams and channels (at least the Java implementation).
        </p>
        <p>
        Warning #1: The current API is a very early version and is likely to change.
        That said barring a massive outcry the core components should remain fairly consistent. Both the core and file projects will be changing but file is even more likely to change.
        Especially with regard to FileAttributes and the API for implementing custom filesystems.
        </p><p>
        Warning #2: I have spent no time on optimization so there are a large number of
        known inefficiencies.  Patches and suggestions are welcome but please keep flaming to a minimum.
        </p><p>
        If you have any suggestions please open a ticket at
        <a href="https://github.com/jesseeichar/scala-io/issues">https://github.com/jesseeichar/scala-io/issues</a>
        </p>
        <div>
        I welcome patches, bug reports and suggestions for this documentation or for the libraries themselves.
        </div><p>
        The forum for discussions is the scala-incubator users group (at least for now)
        <a href="http://groups.google.com/group/scala-incubator">http://groups.google.com/group/scala-incubator</a>
        </p>
        <p>
          If you are interested at looking at the code, you are welcome to take a look at:
          <a href="https://github.com/jesseeichar/scala-io/issues">https://github.com/jesseeichar/scala-io</a>
        </p>
        As an example of what Scala IO brings to the table the following examples compare Java IO vs Scala IO performing
        a simple task of reading data from two URLs and writing them to a file. (I found it amusing that I actually messed up
        the Java example the first try and had to debug it).
        </div>
        <div class="example">
          <h3>Scala</h3>
          <p><div class="example_summary"></div></p>
          <div class="example_code">
            <pre class='brush: scala'>{scalaExample}</pre>
          </div>
        </div>
        <div class="example">
          <h3>Java</h3>
          <p><div class="example_summary"></div></p>
          <div class="example_code">
            <pre class='brush: scala'>{javaExample}</pre>
          </div>
        </div>
      </span>

    Template(true, "Scala IO API Documentation")(
        Template.cssAndJs("./"))(
        <h1>Scala IO API Documentation</h1>)(
        content)(Template.rootNavbar(Link.Overview,projectSites))
  }

}
