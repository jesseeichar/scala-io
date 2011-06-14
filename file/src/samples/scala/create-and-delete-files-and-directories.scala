/**
 * Demonstrate how to create files or directories from a
 * path object.
 */
object CreateAndDeleteFilesAndDirectoriesFromPaths {
  /**
   * Create files and directories
   */
  def create{
    import scalax.file.Path

    val path: Path = Path ("/tmp/file")

    // create file but fail if the file already exists.
    // an exception may be thrown
    path.createFile()

    // force create a file will fail if it is a directory which
    // contain children
    path.createFile(failIfExists=false)

    // TODO createFile with attributes

    // create a directory at the path location
    path.createDirectory()
    path.createDirectory(failIfExists=false)
  }

  /**
   * Delete files and directories
   */
  def delete {
    import scalax.file.Path

    val path: Path = Path ("/tmp/file")

    // Will throw IOException if file could not be deleted
    // even if it cannot be deleted because it does not exist
    path.delete()

    // Will not throw exception if file does not exist but will
    // if it is a non-empty directory or not writeable
    path.deleteIfExists()

    // Delete path and all children.  This is currently not a safe method so
    // it should be used with caution.  Future versions will be better
    // by default it will throw an exception if one of the files cannot be deleted
    path.deleteRecursively()

    // Delete path and all children. If a file cannot be deleted then continue on and delete
    // all that can be deleted
    path.deleteRecursively(true)
    // or
    path.deleteRecursively(continueOnFailure=true)
  }
}
