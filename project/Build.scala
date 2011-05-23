import sbt._

object ScalaIoBuild extends Build
{
	lazy val projects = Seq(root,coreProject, fileProject, coreTestProject, fileTestProject)

	lazy val root = Project("root", file(".")) aggregate(coreProject,coreTestProject,fileProject,fileTestProject)

	lazy val coreProject = Project("core", file("core"))
	lazy val coreTestProject = Project("core-test", file("core-test")) dependsOn(coreProject)
	lazy val fileProject = Project("scala-io-file", file("file")) dependsOn(coreProject)
	lazy val fileTestProject = Project("scala-io-file-test", file("file-test")) dependsOn(fileProject,coreTestProject)
}