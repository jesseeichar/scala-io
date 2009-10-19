/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{File=>JFile}

object FileSystem {
  val defaultFileSystem: FileSystem = new DefaultFileSystem()
}

abstract class FileSystem {
  /**
   * Create a path object for the filesystem
   */
  def apply(path: String): Path
  def roots: List[Path];
}

private[io] class DefaultFileSystem extends FileSystem {
  def apply(path: String): Path = new Path (new JFile (path), this)
  def roots = JFile.listRoots().toList map {f=> Path (f.getPath)}
}