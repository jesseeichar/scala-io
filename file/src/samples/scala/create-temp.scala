/**
 * Creating Temporary Files and directories
 */
object CreateTemporary {

  def createTempFiles {
    import scalax.file.{Path, FileSystem}

    // by default the filesystem is the defaultFileSystem (surprise :-) )
    // using the default parameters will create a randomly named file in
    // the system temp directory which will be deleted when the JVM exists
    val tmpFile1: Path = Path.createTempFile()

    // fully declare the temporary file parameters
    // all parameters have defaults so there are many option
    // Note that not all filesystems support creating temporary
    // files.
    // The default filesystem does
    val tmpFile2: Path = Path.createTempFile(prefix = "tmpFile",
      suffix = "tmp",
      dir = "/tmp",
      deleteOnExit = false)

    // a file system can also be used to create temporary files/directories
    FileSystem.default.createTempFile()
  }

  def createTempDirectories {
    // Note: Both createTempFile and createTempDirectory have the same parameters
    import scalax.file.{Path, FileSystem}

    // by default the filesystem is the defaultFileSystem (surprise :-) )
    // using the default parameters will create a randomly named directory in
    // the system temp directory which will be deleted when the JVM exists
    val tmpFile1: Path = Path.createTempDirectory()

    // fully declare the temporary directory parameters
    // all parameters have defaults so there are many option
    // Note that not all filesystems support creating temporary
    // files/directories.
    // The default filesystem does
    val tmpFile2: Path = Path.createTempDirectory(prefix = "tmpFile",
      suffix = "tmp",
      dir = "/tmp",
      deleteOnExit = false)

    // a file system can also be used to create temporary files/directories
    FileSystem.default.createTempFile()
  }

}
