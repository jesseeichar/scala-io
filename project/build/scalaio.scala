import sbt._

class ScalaProject(info: ProjectInfo) extends ParentProject(info) with IdeaProject {
  val mavenLocal = "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"
  val scalatoolsSnapshot = "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"
  val scalatoolsRelease = "Scala Tools Snapshot" at "http://scala-tools.org/repo-releases/"

  lazy val arm = project("scala-arm", "Arm project")
  lazy val io = project("scala-io", "Core IO project", new ScalaIoCore(_), arm)


  /* ------   Sub projects ------ */
  class ScalaIoCore(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
    val junit = "junit" % "junit" % "4.7" % "test"
    val mockito = "org.mockito" % "mockito-all" % "1.8.0" % "test"
    val junitInterface = "com.novocode" % "junit-interface" % "0.4.0" % "test"

    override def compileOptions = super.compileOptions ++ Seq(Unchecked)
  }

}

