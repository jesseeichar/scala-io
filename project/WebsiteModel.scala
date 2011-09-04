import java.nio.charset.Charset
import sbt.{IO,ExactFilter,PathFinder,PatternFilter}
import xml.Node
import java.io.File


class WebsiteModel(
    val sourcePath:File = new File("."),
    val websiteResources:Seq[File] = List(new File("web-site/src/main/resources")),
    val buildScalaVersion:String = "2.9.0-1",
//    val outputDir:File = new File("target/website")) {
  val outputDir:File = new File("/Users/jeichar/Sites/scala-io")) {

  val version = BuildConstants.version
  val organization = BuildConstants.organization
  val armVersion = BuildConstants.armVersion
  val self = this
  object Dir {
    val app = new File(outputDir,"app")
    val js = new File(app,"js")
  }
  
  def write(to:File,data:String) = {
    IO.write(to, data,Charset.forName("UTF-8"))
  }

  def buildSite = {
    IO.delete(Dir.app)
    websiteResources foreach {dir => IO.copyDirectory(dir,outputDir)}
    val corePages = pages(new File("core"))
    val filePages = pages(new File("file"))
    val keywords = (Keyword.core +: corePages.map(_.keyword)) ++ (Keyword.file +: filePages.map(_.keyword))
    val keywordJSON = keywords.mkString("IO_PAGES=[",",\n\t","]")
    write(new File(Dir.js,"keywords.js"), keywordJSON)
  }

  def read(name:String) = {
    val file = (PathFinder(sourcePath) ** new ExactFilter(name)).get.head
    IO.read(file,Charset.forName("UTF-8"))
  }
  val javaExample = read("JavaIOExample.java")
  val scalaExample = read("ScalaIOExample.scala")

  def pages(projectPath:File):Seq[Page] = {
    val examples = (PathFinder(projectPath) ** new ExactFilter("samples") ** new PatternFilter(".*\\.scala".r.pattern)).get map {
      f => sbt.Path.absolute(f)
    }
    examples.flatMap{path =>
      val example = new Example(projectPath.getName, path)
      example.page +: example.pages
    }
  }  
}

