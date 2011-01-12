import sbt._

class ScalaIOProject(info: ProjectInfo)
        extends ParentProject(info) {
  val mavenLocal = "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"
  val scalatoolsSnapshot = "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"
  val scalatoolsRelease = "Scala Tools Snapshot" at "http://scala-tools.org/repo-releases/"

  lazy val core = project("core", "core", new Core(_))
  lazy val coreTest = project("core-test", "core-test", new TestProject(_),core)
  lazy val file = project("file", "file", new File(_),core)
  lazy val fileTest = project("file-test", "file-test", new TestProject(_),core,coreTest,file)
  lazy val archive = project("archive", "archive", new Archive(_),core, file)


  /* ------   Sub projects ------ */
  class Core(info: ProjectInfo)
          extends DefaultProject(info)
                  with IoProject {

    val scalaArm = "com.github.jsuereth.scala-arm" %% "scala-arm" % "0.2" withSources() withJavadoc()
  }

  class TestProject(info: ProjectInfo)
          extends DefaultProject(info) {

    val mockito = "org.mockito" % "mockito-all" % "1.8.0"
    val junitInterface = "com.novocode" % "junit-interface" % "0.5"

    override def testScalaSourcePath = mainScalaSourcePath
    override def testResourcesPath = mainResourcesPath
  }

  class File(info: ProjectInfo)
          extends DefaultProject(info)
                  with IoProject

  class Archive(info: ProjectInfo)
          extends DefaultProject(info)
                  with IoProject
}

trait IoProject extends AutoCompilerPlugins {
  self : DefaultProject =>

  val cont = compilerPlugin("org.scala-lang.plugins" % "continuations" % buildScalaVersion)
  // val sxr = compilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.6")

  override def compileOptions = super.compileOptions ++ List(
    CompileOption("-P:continuations:enable"),
    //CompileOption("-P:sxr:base-directory:" + mainScalaSourcePath.absolutePath),
    Unchecked)

  /* use default compile options */
  def samplesCompileOptions: Seq[CompileOption] = compileOptions
  /* label it "samples" */
  def samplesLabel = "samples"
  /* look for source under "src/samples" */
  def samplesSourcePath = sourcePath / "samples"
  def samplesSourceRoots = (samplesSourcePath ##)
  def samplesSources = sources(samplesSourceRoots)
  /* compiled classes go under "target/samples-classes" */
  def samplesCompilePath = outputPath / "samples-classes"
  /* analysis output goes under "target/samples-analysis" */
  def samplesAnalysisPath = outputPath / "samples-analysis"
  def samplesClasspath = runClasspath
  def samplesCompileConfiguration = new SamplesCompileConfig
  def samplesCompileConditional = new CompileConditional(samplesCompileConfiguration, buildCompiler)
  def samplesCompileDescription = "Compiles samples."

  class SamplesCompileConfig extends BaseCompileConfig {
    def baseCompileOptions = samplesCompileOptions
    def label = samplesLabel
    def sourceRoots = samplesSourceRoots
    def sources = samplesSources
    def outputDirectory = samplesCompilePath
    def classpath = samplesClasspath
    def analysisPath = samplesAnalysisPath
    def fingerprints = Fingerprints(Nil,Nil)
    def javaOptions = javaCompileOptions.map{o => o.toString}
  }

  protected def compileSamples = task {samplesCompileConditional.run} describedAs samplesCompileDescription

  lazy val samples = compileSamples dependsOn compile

  lazy val compileAll = compileSamples dependsOn (compile,testCompile)
}
