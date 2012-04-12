/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package defaultfs

import java.io.{File=>JFile}
import scalax.io.ResourceContext
import scalax.io.DefaultResourceContext

/**
 * @author  Jesse Eichar
 * @since   1.0
 */
private[file] class DefaultFileSystem(val context:ResourceContext = DefaultResourceContext) extends FileSystem {
  type PathType = DefaultPath
  val name = "Default"
  val separator: String = JFile.separator
  protected def doCreateFromSeq(segments: Seq[String]) = {
    val updatedSegments =
      if (System.getProperty("os.name").toLowerCase.contains("win") && segments.nonEmpty && segments(0) == separator) presentWorkingDirectory.root.getOrElse(roots.head).path +: segments.tail
      else segments
    new DefaultPath(new JFile(updatedSegments mkString separator), this)
  }
  def apply(path: JFile): DefaultPath = fromString(path.getPath)
  def roots = JFile.listRoots().toSet.map {(f:JFile) => fromString (f.getPath)}
  def createTempFile(prefix: String = randomPrefix,
                   suffix: String = null,
                   dir: String = null,
                   deleteOnExit : Boolean = true
                   /*attributes:List[FileAttributes] TODO */ ) : DefaultPath = {
    val dirFile = if(dir==null) null else new JFile(dir)
    val path = fromString(JFile.createTempFile(prefix, suffix, dirFile).getPath)
    if(deleteOnExit) path.jfile.deleteOnExit
    path
  }

  def createTempDirectory(prefix: String = randomPrefix,
                        suffix: String = null,
                        dir: String = null,
                        deleteOnExit : Boolean = true
                        /*attributes:List[FileAttributes] TODO */) : DefaultPath = {
    val path = createTempFile(prefix, suffix, dir, false)
    path.delete(force=true)
    path.createDirectory()
    if(deleteOnExit) {
      Runtime.getRuntime.addShutdownHook(new Thread{override def run:Unit = path.deleteRecursively(true) })
    }
    path
  }
  def updateContext(newContext:ResourceContext):DefaultFileSystem = new DefaultFileSystem(newContext)
  override def updateContext(f:ResourceContext => ResourceContext):DefaultFileSystem = updateContext(f(context))
  override def toString = "Default File System"
}
