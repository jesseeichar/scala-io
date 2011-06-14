import sbt._
import Keys._

object ScalaIoBuild extends Build {
  // ----------------------- Root Project ----------------------- //

	lazy val root:Project = Project("root", file(".")).
    aggregate(coreProject,fileProject).
    settings(sharedSettings ++ Seq(publishArtifact := false) :_*)

  // ----------------------- Samples Settings ----------------------- //

  lazy val Samples = config("samples") extend (Compile)
  val samplesSettings = inConfig(Samples)(Defaults.configSettings) ++ Seq[Setting[_]](
    compile in Test <<= (compile in Test).dependsOn(compile in Samples)
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
    organization := "com.github.scala-incubator.io",
    version := "0.2.0-SNAPSHOT",
    maxErrors := 20,
    scalacOptions += "-deprecation",
    offline := false,
    scalaVersion := "2.9.0-1",
    publishToSettings,
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
    pomExtraSetting
  )

  // ----------------------- Core Project ----------------------- //
  val coreSettings = Seq[Setting[_]](
    name := "scala-io-core",
    libraryDependencies += "com.github.jsuereth.scala-arm" % "scala-arm_2.9.0" % "0.2" withSources(),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.6" % "test",
    publishArtifact in Test := true
  )
	lazy val coreProject = Project("core", file("core")).
    configs(Samples).
	  settings(samplesSettings ++ sharedSettings ++ coreSettings : _*)
  // ----------------------- File Project ----------------------- //
  val fileSettings: Seq[Setting[_]] = Seq(
    name := "scala-io-file",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.6" % "test",
    publishArtifact in Test := true
  )
	lazy val fileProject = Project("file", file("file")).
	  settings (samplesSettings ++ sharedSettings ++ fileSettings : _*).
	  dependsOn(coreProject, coreProject % "test->test")

}