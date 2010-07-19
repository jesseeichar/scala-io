import sbt._

class ScalaIoCore(info: ProjectInfo) extends DefaultProject(info)
{
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  
  val junit = "junit" % "junit" % "4.7" % "test"
  val mockito = "org.mockito" % "mockito-all" % "1.8.0" % "test"
  val junitInterface = "com.novocode" % "junit-interface" % "0.4.0" % "test"
  
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}