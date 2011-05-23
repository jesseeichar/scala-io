import sbt._
import xml.Node
import java.io._

class ScalaIOProject(info: ProjectInfo)
        extends ParentProject(info) with posterous.Publish {

  val parentProject = this;
  val mavenLocal = "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"
  val scalatoolsSnapshot = "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"
  val scalatoolsRelease = "Scala Tools Snapshot" at "http://scala-tools.org/repo-releases/"

  val armVersion = "0.2"


  lazy val core:Core = project("core", "core", new Core(_))
  lazy val coreTest:TestProject = project("core-test", "core-test", new TestProject(_),core)
  lazy val file:File = project("file", "file", new File(_),core)
  lazy val fileTest:TestProject = project("file-test", "file-test", new TestProject(_),core,coreTest,file)
  lazy val perf:PerfProject = project("perf", "perf", new PerfProject(_),core)
//  lazy val archive:Archive = project("archive", "archive", new Archive(_),core, file)
  lazy val webSite:WebSite = project("web-site", "web-site", new WebSite(_),core, file)

  /* ------   Sub projects ------ */
  class Core(info: ProjectInfo)
          extends DefaultProject(info)
                  with IoProject {

    val scalaArm = "com.github.jsuereth.scala-arm" %% "scala-arm" % armVersion withSources() withJavadoc()
    def descSummary = "An idiomatic IO library for Scala"
    def description =
      <span>
        <p>The main goals of Scala IO is to provide a scalable solution to IO for Scala.  In more
        concrete terms, the desire is to be able to both easily and quickly access the data from a
        data source like a file or URL as well as provide the options to micro manage how the data is
        accessed when performance is of the utmost importance.</p>
        <p>The first aspect is to provide an ARM (Automatic Resource Management) solution so resources
        are automatically closed after use.  The design for ARM is essentially the loaner pattern where
        the framework opens a resource and passes the resource to a codeblock/function allowing the
        function to only concern itself with the data access logic and the ARM implementation will guarantee
        that the resource is closed after the access, irregardless of whether an error occurred or not.</p>
        <p>The second aspect is to provide lazy collection style access to an underlying resource allowing
        the skills obtained using the Scala collections library to be used to quickly implement solutions
        as if the resource is simply a collection.  The underlying implementation will ensure the resource
        is closed and will ensure that only the necessary data is loaded.  For example calls to drop will
        skip bytes when possible and take will close the connection after the requested data is obtained</p>
        <p>The third aspect is, as of 0.1 not yet implemented, is asynchronous data access in a simple manner.
        The first solution that is provided has been popularized by node js and is essentially the ability
        to register callbacks with a resource and they can, when possible, be executed in a single thread with
        a single connection.</p>
        <p>The fourth aspect (also not implemented for 0.1) is an iteree pattern for IO.  This design will
        will be the basis of the async aspect and will have two levels of complexity.  The first will be
        a simple function callback to obtain all data and the second will be the ability to return a Done
        event to short-circuit the data loading.</p>
        <p>The final piece of the puzzle is the provide a consistent manner for handling exceptions.  The
        initial implementation simply throws exceptions as they occur but it will be possible to register an
        exception handle with your ARM resource to control how to handle exceptions.
        </p>
      </span>
  }

  class PerfProject(info: ProjectInfo)
          extends DefaultProject(info)  {
    val sperf = "com.github.jsuereth" %% "sperformance" % "0.1"
  }

  class TestProject(info: ProjectInfo)
          extends DefaultProject(info)  with MavenPublishing {

    val mockito = "org.mockito" % "mockito-all" % "1.8.0"
    val junitInterface = "com.novocode" % "junit-interface" % "0.6"

    override def testScalaSourcePath = mainScalaSourcePath
    override def testResourcesPath = mainResourcesPath

  }

  class File(info: ProjectInfo)
          extends DefaultProject(info)
                  with IoProject {
    def descSummary = "An adaptation of the Java NIO2 Filesystem to Scala"
    def description =
      <span>
          The Scala IO File subproject is an adaptation of the Java NIO2 Filesystem to Scala.
          While the main inspiration was NIO2 because of its flexibility and design considerations
          for cross platform filesystem, the actual APIs have diverged in order to be more idiomatic
          Scala.
        <p>
          <em>Note:</em> At the moment scala.io.file is still very volatile the client APIs should be
          fairly stable (for a 0.1.0 version) but the API facing the implementer will likely change dramatically as more
          of the issues are encountered through implementation of new filesystems.  So if you wish
          to implement a filesystem I would like the feedback but be aware that it will require migration
          as the API matures.
        </p><p>
          The Filesystem API consists of two APIs one for the implementer of filesystems and one for the
          user of the Filesystem API.  As described earlier the FileSystem API originated from the Java 7
          NIO2 filesystem and almost all the capability of that API are present in the scala.io.file API.
          However there are still aspects that I have not had time to add to scala.io.file.  A few
          examples of features that will be added are:
          <ul>
            <li>Generic File Attributes, allowing filesystem specific file attributes</li>
            <li>File System Events</li>
            <li>Possibly add the concept of FileStore as in Java 7 NIO2</li>
            <li>Plugin system for discovery of FileSystems</li>
            <li>Secure PathSets which are analogous to SecureDirectoryStreams in NIO2</li>
            <li>Better support for links</li>
          </ul>
        </p>
      </span>
  }

  class Archive(info: ProjectInfo)
          extends DefaultProject(info)
                  with IoProject {
    def descSummary = "An adaptation of the Java NIO2 Filesystem to Scala"
    def description =
      <span>

      </span>
  }

  class WebSite(info:ProjectInfo)
          extends DefaultProject(info) {
    val siteOutput = outputPath / "site"
    val siteZip = outputPath / "site.zip"

    // Needed so that samples task for all project is executed before site
    lazy val samples = task {None}

    def siteTask = task {
      val projectSites = dependencies flatMap {
        case project:DefaultProject with IoProject => List(new ProjectSite(project,log))
        case _ => Nil
      }
      siteOutput.asFile.mkdirs
      FileUtilities.copy(mainResources.get, siteOutput, log)
      projectSites.toList foreach { projectSite =>
        val project = projectSite.project
        val docs = project.mainDocPath

        FileUtilities.copy((project.docPath / "main" / "api").##.***.get, siteOutput / projectSite.name / "scaladoc", log)
      }

      val site = new WebsiteModel(this,parentProject,projectSites.toList,siteOutput,log)
      site.buildSite

      FileUtilities.zip(siteOutput.##.get,siteZip,true,log)
      
      None
    } dependsOn (samples,doc)
    lazy val site = siteTask describedAs "Generate documentation web-site"

    override protected def docAction = task{None}
    override protected def docTestAction = task{None}

/*    val siteArtifact = Artifact(name+"-"+version, "zip", "zip")
    override def artifacts = Set(siteArtifact)                                        */
    override def packageToPublishActions =  super.packageToPublishActions ++ Seq(site)
  }
}

trait IoProject extends AutoCompilerPlugins with MavenPublishing {
  self : DefaultProject =>

  override def documentOptions = super.documentOptions //++ List(CompoundDocOption("-doc-source-url","https://github.com/scala-incubator/scala-io/raw/master/"))
  if(System.getProperty("file.encoding")==null || !List("utf8","utf-8").contains(System.getProperty("file.encoding").toLowerCase)) {
    println("file.encoding must be utf8 or utf-8.  Add -Dfile.encoding=UTF8 to your SBT_OPTS")
    exit(1)
  }


  val cont = compilerPlugin("org.scala-lang.plugins" % "continuations" % buildScalaVersion)
  // val sxr = compilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.6")

  def description:Iterable[Node]
  def descSummary:String



  override def compileOptions = super.compileOptions ++ List(
    //CompileOption("-P:continuations:enable"),
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

  override protected def testCompileAction = super.testCompileAction.dependsOn(compile,samples)

}

trait PackageToPublishActions {
  self: TaskManager =>
  def packageToPublishActions:Seq[Task]
}
trait MavenPublishing extends BasicScalaProject {
  self: DefaultProject =>

  // Publishing rules
  override def managedStyle = ManagedStyle.Maven

  // Choose the deployment location based on -SNAPSHOT in the version number
  lazy val publishTo = if(version.toString.endsWith("-SNAPSHOT")) {
     "nexus.scala-tools.org" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  } else {
    "nexus.scala-tools.org" at "http://nexus.scala-tools.org/content/repositories/releases/"
  }

  addMavenCredentialsForServer("nexus.scala-tools.org", "Sonatype Nexus Repository Manager")

  def addMavenCredentialsForServer(serverId : String, name : String)  {
     try {
       val user = System.getProperty(serverId+".user")
       val password = System.getProperty(serverId+".pass")
       if(user!=null) {
         scala.Console.println("Registering credents on " + serverId + " for user " + user)
         Credentials.add(name, serverId, user, password)
       }
     } catch {
       case t => //Ignore errors, just log
          scala.Console.println("Unable to load maven credentials for [" + serverId + "]")
     }
   }
  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  override def packageSrcJar= defaultJarPath("-sources.jar")

  lazy val sourceArtifact = Artifact.sources(artifactID)
  lazy val docsArtifact = Artifact.javadoc(artifactID)
  abstract override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)
  override def pomExtra =
    <licenses>
      <license>
        <name>Scala License</name>
        <url>http://www.scala-lang.org/node/146</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
  
}
trait ScalaBazaar {
  self: DefaultProject =>
  def outputBinPath = (outputPath ##) / "bin"

  def ouputLibPath = (outputPath ##) / "lib"

  def sbazName = name

  def versionlessJarName = sbazName + ".jar"

  def versionlessJarPath = ouputLibPath / versionlessJarName

  def bazaarPackageName = sbazName + "-" + version + ".sbp"

  def bazaarPackagePath = (outputPath ##) / bazaarPackageName

  def bazaarAdvertName = sbazName + "-" + version + ".advert"

  def bazaarAdvertPath = (outputPath ##) / bazaarAdvertName

  def outputMetaPath = (outputPath ##) / "meta"

  def descriptionPath = outputMetaPath / "description"

  def outputDocPath = (outputPath ##) / "doc"

  def bazaarDepends: List[String] = Nil

  def description: String

  def bazaarPackageBaseURL: String

  lazy val sbazPack = sbazPackTask(bazaarDepends, Some(description))

  def sbazPackTask(depends: List[String], description: Option[String]) = task {
    if (!outputMetaPath.asFile.exists)
      outputMetaPath.asFile.mkdir

    val pack = <package>
      <name>{sbazName}</name>
      <version>{version}</version>{if (!depends.isEmpty)
        <depends>{for (depend <- depends)
        yield <name>{depend}</name>}</depends>
      else
        Nil}{if (!description.isEmpty)
        <description>{description.get}</description>
      else
        Nil}</package>

    val advert = <availablePackage>
      {pack}
      <link>{bazaarPackageBaseURL + bazaarPackageName}</link>
    </availablePackage>

    writeFile(descriptionPath.asFile, pack.toString)
    writeFile(bazaarAdvertPath.asFile, advert.toString)

    FileUtilities.zip(List(outputBinPath, ouputLibPath, outputDocPath, outputMetaPath),
      bazaarPackagePath, true, log)
    None
  }.dependsOn(compile, doc)


  private def writeFile(file: File, content: String) =
    if (file.exists() && !file.canWrite())
      error("File " + file + " is not writable")
    else {
      val writer = new FileWriter(file, false)
      writer.write(content)
      writer.close()
    }
}

