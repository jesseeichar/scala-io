import java.nio.charset.Charset
import sbt.{IO,ExactFilter,PathFinder,PatternFilter}
import sbt.Path.richFile
import xml.Node
import java.io.File
import File.{separator=>sep}
import BuildConstants._

object C {
  val utf8 = Charset.forName("UTF-8")
}

import C.utf8

class WebsiteModel(
    sourcePath:File,
    websiteResources:File,
    buildScalaVersion:String,
    docDirectory:File,
    indexDir:File) {

println("SourcePath = "+sourcePath)

  val version = BuildConstants.version
  val organization = BuildConstants.organization
  val armVersion = BuildConstants.armVersion
  val self = this
  object Dir {
    val app = new File(indexDir,version)
  val examples = new File(app, "example_projects")
    val js = new File(app,"js")
    val core = new File(app,"core")
    val file = new File(app,"file")
    val api = new File(app,"api")
    val overview = new File(app,"overview")
    val performance = new File(app,"performance")
  }

  def write(to:File,data:String) = {
    IO.write(to, data,utf8)
  }

  def buildSite = {
    println("Building website at "+Dir.app)

    IO.delete(Dir.app)
    indexDir.listFiles.filter(f => f.isDirectory && f.getName.endsWith("-SNAPSHOT")).
      foreach (IO.delete)
    Dir.app.mkdirs

    ExampleProjects.prepare(new File(sourcePath, "src"+sep+"main"+sep+Dir.examples.name), Dir.examples)

    websiteResources.listFiles foreach {
      case r if r.getName == "app" =>
        IO.copyDirectory(r,Dir.app)
      case r if r.isDirectory =>
        IO.copyDirectory(r,indexDir)
      case r =>
        IO.copyFile(r, new File(indexDir, r.getName))
    }

    IO.copyDirectory(docDirectory,Dir.api)

    val corePages = pages(new File("core"))
    val filePages = pages(new File("file"))
    val performanceKeywords = Keyword.performance +: PerformanceReport.buildSite(Dir.performance,new File("perf/results/graphs"))
    val keywords = List(Keyword.overview, Keyword.gettingStarted, Keyword.releaseNotes, Keyword.roadmap) ++ (Keyword.core +: corePages.map(_.keyword)) ++ (Keyword.file +: filePages.map(_.keyword)) ++ performanceKeywords
    val keywordJSON = keywords.mkString("IO_PAGES=[",",\n\t","]")
    write(new File(Dir.js,"keywords.js"), keywordJSON)

    corePages.foreach {page =>
      val file = new File(Dir.core,page.keyword.id+".html")
      write(file,page.html.toString)
    }
    filePages.foreach {page =>
      val file = new File(Dir.file,page.keyword.id+".html")
      write(file,page.html.toString)
    }
    write(new File(Dir.js,"projectProperties.js"), projectPropertiesJS)
    writeOverview()
    writeIndexjson()

    println("website build is complete")
  }

  def writeIndexjson() = {
    val names = indexDir.listFiles.toSeq.filter(_.isDirectory).filterNot(_.getName startsWith ".").map(f => "\""+f.getName+"\"")
    val sortedNames = names.sorted.reverse
    val json = sortedNames.mkString("[",",","]")
    IO.write(new File(indexDir,"version-index.json"), json, utf8)

    val html =
    <html>
      <head><meta http-equiv="REFRESH" content={"0;url="+sortedNames(0)+"/index.html"}/></head>
      <body>Redirecting to <a href={sortedNames(0)+"/index.html"}>documentation for latest version</a></body>
    </html>

    IO.write(new File(indexDir, "latest.html"), html.toString, utf8)
  }

  def read(name:String) = {
    val file = (PathFinder(sourcePath) ** new ExactFilter(name)).get.head
    IO.read(file,utf8)
  }
  def writeOverview() = {
    val template = IO.read(new File(Dir.overview,"template.html"),utf8)
    val overview =  template.replace("{{scalaExample}}",scalaExample).replace("{{javaExample}}",javaExample)
    write(new File(Dir.overview,"index.html"), overview)
  }
  lazy val javaExample = (<pre class='brush: scala'>{read("JavaIOExample.java")}</pre>).toString
  lazy val scalaExample = (<pre class='brush: scala'>{read("ScalaIOExample.scala")}</pre>).toString

  def pages(projectPath:File):Seq[Page] = {
    val examples = (PathFinder(projectPath) ** new ExactFilter("samples") ** new PatternFilter(".*\\.scala".r.pattern)).get map {
      f => sbt.Path.absolute(f)
    }
    examples.flatMap{path =>
      val example = new Example(projectPath.getName, path)
      example.page +: example.pages
    }
  }

  def projectPropertiesJS = {
    val snapshotBlurb =
      if (version.endsWith("-SNAPSHOT"))
        "These are directions for a SNAPSHOT Release so the Maven Snapshot Repository (https://oss.sonatype.org/content/repositories/snapshots) must be used instead of the normal releases repo that are in the following directions. Since this is a snapshot release I leave learning how to do that to you."
      else
        ""
    val snapshotDownloadURL =
      if (version.endsWith("-SNAPSHOT"))
        "https://oss.sonatype.org/content/repositories/snapshots/"
      else
        "https://oss.sonatype.org/content/repositories/releases/"

    val properties =
    """|var IO_PROPS = {
       |  groupId: "%s",
       |  version: "%s",
       |  scalaVersion: "%s",
       |  scalaBaseVersion: "%s",
       |  armVersion: "%s",
       |  scalaArmVersion: "%s",
       |  ioMavenPath: "%1$s".replace(/\./g,'/'),
       |  SNAPSHOT_BLURB: "%s",
       |  SNAPSHOT_DOWNLOAD_URL: "%s",
       |};""".format(organization, version, buildScalaVersion, BuildConstants.scalaVersion.take(BuildConstants.scalaVersion.lastIndexOf('.')), armVersion, armScalaVersion, snapshotBlurb, snapshotDownloadURL)

    properties.trim.stripMargin.lines.map(_.trim).mkString
  }
}
