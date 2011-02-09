import sbt.{Logger, Path}
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

class WebsiteModel(val projectSites:List[ProjectSite],val outputDir:Path,log:Logger) {
  val self = this;
  def buildSite = {
    Printer.printPage(outputDir / "index.html",indexHtml,log)
    Printer.printPage(outputDir / "roadmap.html",roadmapHtml,log)
    projectSites.foreach {
      project=>
        project.buildSite(self);
    }
  }

  def navbar(overviewActive:Boolean) =
    <div id="navcontainer">
      <ul id="projectnavlist">
        <li><a href="./index.html" class={if(overviewActive)"active" else ""}>Overview</a></li>
        {for(project <- projectSites) yield {
        <li><a href={project.name+"/index.html"}
               title={project.summary}>{project.name.capitalize}</a> <a href={project.name+"/scaladoc/index.html"}>(Scaladoc)</a>
          <ul id="navlist">
            {for(page <- project.pages) yield {
              <li><a title={page.uberSummaryText} href={project.pagePath(page)}>{page.name}</a></li>
            }
          }</ul>
        </li>
      }}
        <li><a href="roadmap.html" class={if(!overviewActive)"active" else ""}>Roadmap</a></li>
      </ul>
    </div>

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
        <a href="https://github.com/scala-incubator/scala-io/issues">https://github.com/scala-incubator/scala-io/issues</a>

        I will monitor the tickets and reply on the tickets.
        </p>
      </div>


    Template(false, "Scala IO API Documentation")(
        <link href={"css/samples.css"} rel="stylesheet" type="text/css" ></link>)(
        <h1>Scala IO API Documentation</h1>)(
        content)(navbar(true))
  }

  def roadmapHtml = {

    def navbar(overviewActive:Boolean) = <div id="navcontainer">
        <ul id="projectnavlist">
          <li><a href="./index.html" class={if(overviewActive)"active" else ""}>Overview</a></li>
          {for(project <- projectSites) yield {
          <li><a href={project.name+"/index.html"}
                 title={project.summary}>{project.name.capitalize}</a> <a href={project.name+"/scaladoc/index.html"}>(Scaladoc)</a>
            <ul id="navlist">
              {for(page <- project.pages) yield {
                <li><a title={page.uberSummaryText} href={project.pagePath(page)}>{page.name}</a></li>
              }
            }</ul>
          </li>
        }}
          <li><a href="roadmap.html" class={if(!overviewActive)"active" else ""}>Roadmap</a></li>
        </ul>
      </div>

    val roadmapContent =
        <div class="explanation">
          <strong>Scala IO Core</strong>
          <p>Scala IO Core will be completed before much more work is done on the FS API so that the library
          won't be forever being developed.  In addition to the major items listed below the common task of
          improving API and removing inconsistency will always be a focus.  As will performance.</p>
          <ol>
              <li><strong>Large Data Integration Tests</strong>
                  <div>Create test sets that are very large (several GB) and verify that the operations can
                  handle the load and work correctly when skipping data</div>
              </li>
              <li><strong>Write Benchmark Tests</strong>
                  <div> Benchmark the integration tests as well as other types of operations (such as
                      large number of small requests on a large dataset).
                      <br/>
                      The idea is to track the performance of each version and ensure it continues to
                      increase each version (and by how much)
                  </div>
              </li>
              <li><strong>Add error handling to resources</strong>
                  <div>
                      At the moment unless a resource is used via Josh's ARM API an error reading from a
                      stream results in an exception being thrown.  I want to allow a user to add an error
                      handler on the resource itself rather than having to use a catch block each time the
                      resource is used
                  </div>
              </li>
              <li><strong>Add Iteratee style IO processing</strong>
                  <div>
                      This is a very flexible way of handling IO with very good compositional properties
                      but for some (who are not familiar with the pattern) it can be less approachable
                  </div>
              </li>
              <li><strong>Add asynchronous callback style IO handling</strong>
                  <div>
                      This will appear much like that which is seen in NodeJS and will be implemented
                      based on the Iteratee IO processing
                  </div>
              </li>
              <li><strong>Implement Java 7 Implementations</strong>
                  <div>This may be raised in the list of priorities depending on how long each task takes.</div>
              </li>
          </ol>
          <strong>Scala IO File</strong>
          <ol>
              <li><strong>Implement ZipFS</strong>
                  <div>
                      The goal is to have several different filesystem implementations that have different
                      characteristics to insure that the API is sufficiently flexible and, most importantly,
                      to make sure that the file system implementation API is flexible.  The usage API is
                      design following NIO2 so I am fairly confident that has sufficient flexibility but the
                      implementer's API is also a very important API.
                  </div>
              </li>
              <li><strong>Implement based on Java 7</strong></li>
              <li><strong>TBD...</strong></li>
          </ol>
        </div>

    Template(false, "Scala IO Development Roadmap")(
        <link href={"css/samples.css"} rel="stylesheet" type="text/css" ></link>)(
        <h1>Scala IO Development Roadmap</h1>)(
        roadmapContent)(navbar(false))
  }

}
