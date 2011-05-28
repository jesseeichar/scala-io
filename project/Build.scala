import sbt._

object ScalaIoBuild extends Build
{
	lazy val projects = Seq(root,coreProject, fileProject, coreTestProject, fileTestProject, sharedConfig)

	lazy val root:Project = Project("root", file(".")) aggregate(coreProject,coreTestProject,fileProject,fileTestProject)

  lazy val sharedConfig = Project("sharedConfig", file("sharedConfig"))
	lazy val coreProject = Project("core", file("core")) delegateTo sharedConfig
	lazy val coreTestProject = Project("core-test", file("core-test")) dependsOn coreProject delegateTo sharedConfig
	lazy val fileProject = Project("file", file("file")) dependsOn(coreProject) delegateTo sharedConfig
	lazy val fileTestProject = Project("file-test", file("file-test")) dependsOn(fileProject,coreTestProject)  delegateTo sharedConfig
}