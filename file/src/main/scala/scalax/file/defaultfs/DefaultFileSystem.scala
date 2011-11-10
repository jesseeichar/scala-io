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

/**
 * @author  Jesse Eichar
 * @since   1.0
 */
private[file] class DefaultFileSystem extends FileSystem {
  val name = "Default"
  def separator: String = JFile.separator
  def fromString(path: String): DefaultPath = apply (new JFile (path))
  def apply(path: JFile): DefaultPath = new DefaultPath (path, this)
  def roots = JFile.listRoots().toSet.map {(f:JFile) => fromString (f.getPath)}
  def createTempFile(prefix: String = randomPrefix,
                   suffix: String = null,
                   dir: String = null,
                   deleteOnExit : Boolean = true
                   /*attributes:List[FileAttributes] TODO */ ) : Path = {
    val dirFile = if(dir==null) null else new JFile(dir)
    val path = fromString(JFile.createTempFile(prefix, suffix, dirFile).getPath)
    if(deleteOnExit) path.jfile.deleteOnExit
    path
  }

  def createTempDirectory(prefix: String = randomPrefix,
                        suffix: String = null,
                        dir: String = null,
                        deleteOnExit : Boolean = true
                        /*attributes:List[FileAttributes] TODO */) : Path = {
    val path = createTempFile(prefix, suffix, dir, false)
    path.delete(force=true)
    path.createDirectory()
    if(deleteOnExit) {
      Runtime.getRuntime.addShutdownHook(new Thread{override def run:Unit = path.deleteRecursively(true) })
    }
    path
  }

  override def toString = "Default File System"
}
