import scalax.file.ramfs.RamFileSystem

/**
 * Demonstrate creating simple paths and moving them withing the filesystem and
 * to other filesystems.
 */
object CreateAndMoveFilesAndDirectories {
  /**
   * copy and move/rename files
   */
  def copyAndMovePaths{
    import scalax.file.Path

    val path: Path = Path ("/tmp/file")
    val dest: Path = Path ("/tmp/file2")

    // make a copy of the file
    // by default this will fail if dest already exists
    // also attribute information like datestamp will be
    // set on the destination file
    // If path is a directory the copy will not be recursive
    path.copyTo (dest)

    // Copy explicitly declaring options
    path.copyTo (target=dest,
                 copyAttributes=false,
                 replaceExisting=true)

    // Move/Rename the path
    // by default throw exception if destination exists
    // and if a copy is required by underlying filesystem then do that
    path.moveTo (target=dest)

    // Here we will overwrite existing files (but not non-empty directories)
    // and will fail if a copy is required (similar to java.file.File.renameTo)
    // if a failure occures an exception is thrown
    path.moveTo (target=dest,
                 replace=true,
                 atomicMove=true)
  }

  /**
   * Since the underlying filesystem could change to safely use the PathSet API it is recommended to handle the
   * NotDirectoryException
   */
  def notDirectoryException {
    import scalax.file.{Path, NotDirectoryException}
    import scala.util.control.Exception._

    catching (classOf[NotDirectoryException]) opt {
      Path ("/tmp/dir").children() map ( _.name)
    } match {
      case None => println ("Not a direcory")
      case Some(names) => println ("files names = "+names)
    }
  }

  /**
   * Move a file from one filesystem to another
   */
  def moveBetweenFileSystems {
    import scalax.file._
    import ramfs.RamFileSystem

    val fs = RamFileSystem()
    val ramPath = fs("/","tmp")
    val path = Path("file")  // default filesystem

    path.moveTo(ramPath)
  }
}
