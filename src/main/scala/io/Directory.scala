/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{ File => JFile }
import collection.Traversable

object Directory
{  
  def apply(path: Path):Directory    = path.toDirectory
  
  // Like File.makeTemp but creates a directory instead
  def makeTemp(prefix: String = Path.randomPrefix, suffix: String = null, dir: JFile = null): Directory = {
    val path = File.makeTemp(prefix, suffix, dir)
    path.delete()
    path.toDirectory().create()
  }
}
import Path._

/** 
 *  An abstraction for directories.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
class Directory(jfile: JFile) extends Path(jfile)
{  
  override def toDirectory: Directory = this
  override def toFile: File = new File(jfile)
  override def isValid = jfile.isDirectory() || !jfile.exists()
  
  /** An iterator over the contents of this directory.
   */
  def list: Iterator[Path] =
    jfile.listFiles match {
      case null   => Iterator.empty
      case xs     => xs.iterator map Path.apply
    }
  
  def dirs: Iterator[Directory] = list filterMap { case x: Directory => x }
  def files: Iterator[File] = list filterMap { case x: File => x }
  
  def deepList(depth: Int = 1): Iterator[Path] =
    if (depth == 0) Iterator.empty
    else list ++ (dirs flatMap (_ deepList (depth - 1)))
  
  /** An iterator over the directories underneath this directory,
   *  to the (optionally) given depth.
   */
  def subdirs(depth: Int = 1): Iterator[Directory] = 
    deepList(depth) filterMap { case x: Directory => x }
    
  /** Deletes the directory recursively. Returns false on failure.
   *  Use with caution!
   */
  def deleteRecursively(): Boolean = deleteRecursively(jfile)
  private def deleteRecursively(f: JFile): Boolean = {
    if (f.isDirectory) f.listFiles match { 
      case null =>
      case xs   => xs foreach deleteRecursively
    }
    f.delete()
  }
  
  override def toString() = "Directory(%s)".format(path)
  /**
   * Create the directory referenced by this path.  
   * <p>
   * If failIfExists then FileAlreadyExistsException is thrown if the directory already exists
   * </p>
   * @throws FileAlreadyExistsException
   */
  def create(force: Boolean = true, failIfExists: Boolean = false): Directory = {
    val res = if (force) jfile.mkdirs() else jfile.mkdir()
    if (!res && failIfExists && exists) FileAlreadyExistsExcepion("Directory '%s' already exists." format name)
    else if (isDirectory) toDirectory
    else new Directory(jfile)
  }
}
