import sbt._
import Keys._

object ScalaIoBuild extends Build {
  // ----------------------- Root Project ----------------------- //
	lazy val root:Project = Project("root", file(".")) aggregate(coreProject,coreTestProject,fileProject,fileTestProject)

  // ----------------------- Shared Settings ----------------------- //
  val publishToSetting = publishTo <<= (version) { version => 
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
    publishToSetting,
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
    pomExtraSetting
  )

  // ----------------------- Core Project ----------------------- //
  val coreSettings = Seq[Setting[_]](
    name := "scala-io-core",
    libraryDependencies += "com.github.jsuereth.scala-arm" % "scala-arm_2.9.0" % "0.2" withSources()
  )
	lazy val coreProject = Project("core", file("core")).
	  settings(sharedSettings ++ coreSettings : _*)
  // ----------------------- Core Test Project ----------------------- //
  val coreTestSettings = Seq[Setting[_]](
    name := "scala-io-core-test",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.6"
  )
	lazy val coreTestProject = Project("core-test", file("core-test")).
	  settings(sharedSettings ++ coreTestSettings : _*).
	  dependsOn(coreProject)
  // ----------------------- File Project ----------------------- //
  val fileSettings: Setting[_] = name := "scala-io-file"
	lazy val fileProject = Project("file", file("file")).
	  settings (sharedSettings ++ fileSettings : _*).
	  dependsOn(coreProject)
  // ----------------------- File Test Project ----------------------- //
  val fileTestSettings =  Seq[Setting[_]](
    name := "scala-io-file-test"/*,
    dependencyClasspath += "core-test/test:class-directory"*/
  )
        
	lazy val fileTestProject = Project("file-test", file("file-test")).
	  settings(sharedSettings ++ fileTestSettings : _*).
	  dependsOn(fileProject,coreTestProject % "test->test")
	
}