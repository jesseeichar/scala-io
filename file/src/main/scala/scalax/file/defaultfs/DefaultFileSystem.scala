/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package defaultfs

import scala.collection.JavaConverters._
import java.io.{File => JFile}
import java.nio.file.{FileSystem=>JFileSystem, Path => JPath, Files => JFiles}
import scalax.io.ResourceContext
import scalax.io.DefaultResourceContext
import java.nio.file.attribute.FileAttribute

/**
 * @author  Jesse Eichar
 * @since   1.0
 */
private[file] class DefaultFileSystem(jFileSystem: JFileSystem, val context:ResourceContext = DefaultResourceContext) extends FileSystem {
  type PathType = DefaultPath
  val name = "Default"
  val separator: String = jFileSystem.getSeparator
  protected def doCreateFromSeq(segments: Seq[String]) = {
    val updatedSegments =
      if (System.getProperty("os.name").toLowerCase.contains("win") && segments.nonEmpty && segments(0) == separator) presentWorkingDirectory.root.getOrElse(roots.head).path +: segments.tail
      else segments
    new DefaultPath(jFileSystem.getPath(updatedSegments.head, updatedSegments.tail:_*), this)
  }
  def apply(path: JPath): DefaultPath = fromString(path.toString)
  def apply(path: JFile): DefaultPath = fromString(path.getPath)
  def roots = JFile.listRoots().toSet.map {(f:JFile) => fromString (f.getPath)}
  override def createTempFile(prefix: String,
                   suffix: Option[String],
                   dir: Option[String],
                   deleteOnExit: Boolean,
                   attributes:Set[FileAttribute[_]] ) : DefaultPath = {

    val jpath = dir match {
      case Some(dir) => JFiles.createTempFile(jFileSystem.getPath(dir), prefix, suffix getOrElse null, attributes.toSeq:_*)
      case None => JFiles.createTempFile(prefix, suffix getOrElse null, attributes.toSeq:_*)
    }
    
    val path = apply(jpath)
    if (deleteOnExit) FileSystem.deleteOnShutdown(path)
    path
  }

  override def createTempDirectory(prefix: String,
                        dir: Option[String],
                        deleteOnExit: Boolean,
                        attributes:Set[FileAttribute[_]] ) : DefaultPath = {
    val jpath = dir match {
      case Some(dir) => JFiles.createTempDirectory(jFileSystem.getPath(dir), prefix, attributes.toSeq:_*)
      case None => JFiles.createTempDirectory(prefix, attributes.toSeq:_*)
    }
    
    val path = apply(jpath)
    if (deleteOnExit) FileSystem.deleteOnShutdown(path)
    path

  }
  override def supportedFileAttributeViews: Set[String] = jFileSystem.supportedFileAttributeViews.asScala.toSet
  def updateContext(newContext:ResourceContext):DefaultFileSystem = new DefaultFileSystem(jFileSystem, newContext)
  override def updateContext(f:ResourceContext => ResourceContext):DefaultFileSystem = updateContext(f(context))
  override def toString = "Default File System"
}
