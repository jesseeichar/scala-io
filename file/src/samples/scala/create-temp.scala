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
      deleteOnExit = false)(FileSystem.default)

    // Using the same pattern as Path you can can use implicits
    // to declare the FileSystem that is used by make temp file
    implicit val fs = FileSystem.default
    // fs will now be used by createTempFile
    val tmpFile3: Path = Path.createTempFile()

    // a file system can also be used to create temporary files/directories
    fs.createTempFile()
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
      deleteOnExit = false)(FileSystem.default)

    // Using the same pattern as Path you can can use implicits
    // to declare the FileSystem that is used by make temp directory
    implicit val fs = FileSystem.default
    // fs will now be used by createTempDirectory
    val tmpFile3: Path = Path.createTempDirectory()

    // a file system can also be used to create temporary files/directories
    fs.createTempFile()
  }

}
