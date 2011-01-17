object CreatePath {

  // Create a Path in the default filesystem explicitly
  def explicitCreation = {
    import scalax.file.{Path, FileSystem}
    // first use default param to indicate defaultFileSystem
    val path1: Path = Path ("/tmp/file1")

    // include windows examples for completeness
    val path2: Path = Path ("file://c:/tmp/file2")
    val path3: Path = Path ("file://c:\\tmp\\file3")

    //now explicitly state the filesystem
    val path4: Path = Path ("/tmp/file4")(FileSystem.default)

    // or declare an implicit val so it can be reused (this is bot really
    // required since the default paramter is the default filesystem but
    // it illustrates how another filesystem can be used
    implicit val fs = FileSystem.default
    // fs will now be used to create the path
    val path5: Path = Path ("/tmp/file5")

    // a filesystem can also be used to create Paths
    val path6: Path = fs ("/tmp/file6")
  }

  // Create Path from URI
  def fromURI = {
    import scalax.file.{Path}
    import java.net.URI
    // the URI type indicates which filesystem to use
    // file:// indicates the default filesystem
    val path1: Option[Path] = Path (new URI ("file:///tmp/file1"))

    // include windows examples for completeness
    val path2: Option[Path]= Path (new URI ("file://c:/tmp/file2"))
    val path3: Option[Path]= Path (new URI ("file://c:\\tmp\\file3"))

    // For opening a zip filesystem
    val zipPath: Option[Path] = Path (new URI ("zip:///tmp/zipfile.zip!/file"))
  }

  // TODO demonstrate the GenericPath usage

  // Create path from java.file.File.
  def fromJFile = {
    import java.io.File
    import scalax.file.Path
    // java.file.File are always on the default filesystem
    // so filesystem is not declared
    val path1: Path = Path (new File ("/tmp/file1"))
    // include windows examples for completeness
    val path2: Path = Path (new File ("file://c:/tmp/file2"))
    val path3: Path = Path (new File ("file://c:\\tmp\\file3"))
  }

}
