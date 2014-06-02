import sbt._
import Keys._
import scala.xml.transform._

object BuildConstants {
  val organization = "com.github.scala-incubator.io"
  val version = "0.4.3-1"
  val armVersion = "1.3.3"
  val armScalaVersion = "2.11"
  val scalaVersion = "2.11.0"
}

object ScalaIoBuild extends Build {
  // ----------------------- Root Project ----------------------- //

  lazy val root:Project = Project("root", file(".")).
    aggregate(coreProject,fileProject,webSiteProject).
    settings(sharedSettings ++ Seq(publishArtifact := false, name := "Scala IO") :_*)

  // ----------------------- Samples Settings ----------------------- //

  lazy val Samples = config("samples") extend (Compile)
  val samplesSettings = inConfig(Samples)(Defaults.configSettings) ++ Seq[Setting[_]](
    compile in Test <<= (compile in Test).dependsOn(compile in Samples)
    //dependencyClasspath in Samples <<= (dependencyClasspath in Test)
  )

  // ----------------------- Shared Settings ----------------------- //
  val publishToSettings = publishTo <<= version { v: String =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
    else                             Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }

  val pomExtraSetting = pomExtra := (
    <scm>
      <url>git@github.com:jesseeichar/scala-io.git</url>
      <connection>scm:git:git@github.com:jesseeichar/scala-io.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jesseeichar</id>
        <name>Jesse Eichar</name>
        <url>http://www.pragmaticdesign.com</url>
      </developer>
    </developers>
  )

  val sharedSettings = Seq[Setting[_]](
     //scalaHome := Some(file("/Volumes/Box/ScalaProject/scala-full/dists/scala-2.9.2.r25667-b20110921211926")),
    organization := BuildConstants.organization,
    version := BuildConstants.version,
    licenses := Seq("Scala License" -> url("http://www.scala-lang.org/node/146")),
    homepage := Some(url("http://jesseeichar.github.com/scala-io-doc/index.html")),
    maxErrors := 20,
    scalacOptions ++= Seq("-optimize","-deprecation"),
    offline := false,
//    parallelExecution in Test := false,
    scalaVersion := BuildConstants.scalaVersion,
  
    publishMavenStyle := true,
    publishToSettings,
    pomExtraSetting,
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    resolvers += "Java.net" at "http://download.java.net/maven/2/",
    resolvers += {
      val mapfishRepoUrl = new java.net.URL("http://dev.mapfish.org/ivy2")
      Resolver.url("Mapfish Ivy Repository", mapfishRepoUrl)(Resolver.ivyStylePatterns)
    },
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11-RC1" % "test->default" exclude ("org.scala-lang.modules", "scala-parser-combinators_2.11"),
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.1",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % BuildConstants.scalaVersion % "test"
  )

  // ----------------------- Core Project ----------------------- //

  val coreSettings = Seq[Setting[_]](
    name := "scala-io-core",
    libraryDependencies += "com.madgag" % ("scala-arm_"+BuildConstants.armScalaVersion) % BuildConstants.armVersion,
    publishArtifact in Test := true
  )
  lazy val coreProject = Project("core", file("core")).
    configs(Samples).
    settings(samplesSettings ++ sharedSettings ++ coreSettings : _*)
  // ----------------------- File Project ----------------------- //


  val removeScalaIOTestDependency = new RuleTransformer(new RewriteRule {
    override def transform(n: xml.Node) = {
      val isBadDependencyElem = (n.label == "dependency") &&
        (n \ "artifactId").text.startsWith("scala-io-core") &&
        (n \ "groupId").text.equals(BuildConstants.organization) &&
        (n \ "scope").text.equals("test")
      if (isBadDependencyElem) Nil
      else n
    }
  })

  // add scala-parser-combinators dependency when needed (for Scala 2.11 and newer) in a robust way
  // this mechanism supports cross-version publishing
  private val addScalaParserCombinatorsModule = libraryDependencies := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, add dependency on scala-parser-combinators module
      case Some((2, scalaMajor)) if scalaMajor >= 11 =>
        libraryDependencies.value :+ "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
      case _ =>
        libraryDependencies.value
    }
  }

  val fileSettings: Seq[Setting[_]] = Seq(
    name := "scala-io-file",
    pomPostProcess := removeScalaIOTestDependency.apply,
    addScalaParserCombinatorsModule
  )
  lazy val fileProject = Project("file", file("file")).
    configs(Samples).
    settings (samplesSettings ++ sharedSettings ++ fileSettings : _*).
    dependsOn(coreProject, coreProject % "test->test")

  // ----------------------- Performace Project ---------------------//

  val perfSettings: Seq[Setting[_]] = Seq(
    name := "scala-io-performance",
    libraryDependencies += "com.github.jsuereth" % "sperformance_2.10" % "0.1",
    publishArtifact := false
  )
  /*lazy val perfProject = Project("perf", file("perf")).
    settings (samplesSettings ++ sharedSettings ++ perfSettings : _*).
    dependsOn(coreProject,coreProject % "compile->test", fileProject % "compile->test")
*/

  // ------------------------------ Docs Project ------------------------------ //
  lazy val docsSite = TaskKey[Unit]("docs-site","Generate documentation web-site")
  lazy val siteDir = TaskKey[File]("site-dir","Directory of the generated website")
  lazy val SiteTask = docsSite in Docs <<= (siteDir,baseDirectory,scalaVersion,resourceDirectory,target in doc) map {
    (out,baseDirectory,scalaVersion,resourceDirectory,docDirectory) =>

    val model = new WebsiteModel(
    sourcePath = baseDirectory,
    websiteResources = resourceDirectory,
    docDirectory = docDirectory,
    buildScalaVersion = BuildConstants.scalaVersion,
    indexDir = out)

    model.buildSite
  }
  lazy val Docs = config("docs") extend (Compile)
  val docsSettings = inConfig(Docs)(Defaults.configSettings) ++ Seq[Setting[_]](
      name := "scala-io-docs",
      scalacOptions in Docs ++= Seq("-doc-title", "Scala IO"),//, "–doc-source-url", "https://raw.github.com/jesseeichar/scala-io/master/core/src/main/scala/"),
      resourceDirectory := new File("documentation/src/main/resources"),
      //siteDir <<= baseDirectory map { base => new File(base, "target/website") },
      siteDir := new File("/Users/jeichar/Sites/scala-io-doc/"),
      SiteTask,
      docsSite in Docs <<= (docsSite in Docs).dependsOn(doc in Docs),
      sources in Docs <<=
        (sources in (coreProject,Compile),
        sources in (fileProject,Compile)) map { _ ++ _ }
    )
  lazy val webSiteProject:Project = Project("docs", file("documentation")).
    dependsOn(coreProject, fileProject, fileProject % "docs->compile").
    settings(sharedSettings ++ docsSettings :_*)

}
