import sbt._

class ScalaIoCore(info: ProjectInfo) extends DefaultProject(info)
{
  val junit = "junit" % "junit" % "4.7" % "test"
  val mockito = "org.mockito" % "mockito-all" % "1.8.0" % "test"
  val junitInterface = "com.novocode" % "junit-interface" % "0.4" % "test"
  
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}