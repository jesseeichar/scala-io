import sbt._
import Keys._

object BuildConstants {
  val organization = "com.github.scala-incubator.io"
  val version = "0.3.0-SNAPSHOT"
  val armVersion = "0.3"
  val scalaVersion = "2.9.1"
}

object ScalaIoBuild extends Build {
  // ----------------------- Root Project ----------------------- //

	lazy val root:Project = Project("root", file(".")).
    aggregate(coreProject,fileProject,perfProject,webSiteProject).
    settings(sharedSettings ++ Seq(publishArtifact := false) :_*)

  // ----------------------- Samples Settings ----------------------- //

  lazy val Samples = config("samples") extend (Compile)
  val samplesSettings = inConfig(Samples)(Defaults.configSettings) ++ Seq[Setting[_]](
    compile in Test <<= (compile in Test).dependsOn(compile in Samples)
    //dependencyClasspath in Samples <<= (dependencyClasspath in Test)
  )

  // ----------------------- Shared Settings ----------------------- //
  val publishToSettings = publishTo <<= (version) { version =>
    if(version.toString endsWith "-SNAPSHOT")
      Some("nexus.scala-tools.org" at "http://nexus.scala-tools.org/content/repositories/snapshots/")
    else
      Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/")
  }

  val pomExtraSetting = pomExtra :=
    <licenses>
      <license>
        <name>Scala License</name>
        <url>http://www.scala-lang.org/node/146</url>
        <distribution>repo</distribution>
      </license>
    </licenses>

  val sharedSettings = Seq[Setting[_]](
   	scalaHome := Some(file("/Volumes/Box/ScalaProject/scala-full/dists/scala-2.9.2.r25667-b20110921211926")),
    organization := BuildConstants.organization,
    version := BuildConstants.version,
    maxErrors := 20,
    scalacOptions ++= Seq("-optimize","-deprecation"),
    offline := false,
    scalaVersion := BuildConstants.scalaVersion,
    publishToSettings,
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
    pomExtraSetting,
    resolvers += {
      val mapfishRepoUrl = new java.net.URL("http://dev.mapfish.org/ivy2")
      Resolver.url("Mapfish Ivy Repository", mapfishRepoUrl)(Resolver.ivyStylePatterns)
    },
    libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test->default",
    publishArtifact in Test := true
  )

  // ----------------------- Core Project ----------------------- //
  val coreSettings = Seq[Setting[_]](
    name := "scala-io-core",
    libraryDependencies += "com.github.jsuereth.scala-arm" %% "scala-arm" % BuildConstants.armVersion withSources()
  )
	lazy val coreProject = Project("core", file("core")).
    configs(Samples).
	  settings(samplesSettings ++ sharedSettings ++ coreSettings : _*)
  // ----------------------- File Project ----------------------- //
  val fileSettings: Seq[Setting[_]] = Seq(
    name := "scala-io-file"
  )
	lazy val fileProject = Project("file", file("file")).
    configs(Samples).
	  settings (samplesSettings ++ sharedSettings ++ fileSettings : _*).
	  dependsOn(coreProject, coreProject % "test->test")
    
  // ----------------------- Performace Project ---------------------//
  
  val perfSettings: Seq[Setting[_]] = Seq(
    name := "scala-io-performance",
    libraryDependencies += "com.github.jsuereth" %% "sperformance" % "0.1",
    publishArtifact in Test := false
  )
	lazy val perfProject = Project("perf", file("perf")).
	  settings (samplesSettings ++ sharedSettings ++ perfSettings : _*).
	  dependsOn(coreProject,coreProject % "compile->test", fileProject % "compile->test")
  

  // ------------------------------ Docs Project ------------------------------ //
  lazy val site = TaskKey[Unit]("site","Generate documentation web-site")
  lazy val siteDir = TaskKey[File]("site-dir","Directory of the generated website")
  lazy val SiteTask = site in Docs <<= (siteDir,baseDirectory,scalaVersion,resourceDirectory,docDirectory in Docs) map {
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
      scalacOptions in Docs ++= Seq("-doc-title", "Scala IO"),//, "–doc-source-url", "https://raw.github.com/jesseeichar/scala-io/master/core/src/main/scala/"),
      resourceDirectory := new File("documentation/src/main/resources"),
      siteDir <<= baseDirectory map { base => new File(base, "target/website") },
      //siteDir := new File("/Users/jeichar/Sites/scala-io-doc/"),
      SiteTask,
      site in Docs <<= (site in Docs).dependsOn(doc in Docs),
      sources in Docs <<=
        (sources in (coreProject,Compile),
        sources in (fileProject,Compile)) map { _ ++ _ }
    )
  lazy val webSiteProject:Project = Project("documentation", file("documentation")).
    dependsOn(fileProject, fileProject % "docs->compile").
    settings(sharedSettings ++ docsSettings :_*)
}