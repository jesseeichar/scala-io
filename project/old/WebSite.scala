import sbt.{PatternFilter, IO, ExactFilter, PathFinder}
import xml.Node
import java.io.File

trait ProjectDescription {
  def descSummary:String
  def description:Node
  def name:String
  def samplesSources:Seq[File] = (PathFinder(projectPath) ** new ExactFilter("samples") ** new PatternFilter(".*\\.scala".r.pattern)).get map {
    f => sbt.Path.absolute(f)}
  def projectPath:File
}
object CoreSite extends ProjectDescription {
  def name = "Scala IO Core"
  def descSummary = "An idiomatic IO library for Scala"
  def projectPath = new File("core")
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

object FileSite extends ProjectDescription {
  def name = "Scala IO File"
  def projectPath = new File("file")
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

