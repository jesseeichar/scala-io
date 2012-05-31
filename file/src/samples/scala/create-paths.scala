/**
 * Demonstrate various ways of creating Path objects
 */
object CreatePath {

  // Create a Path in the default filesystem explicitly
  def explicitCreation = {
    import scalax.file.{Path, FileSystem}
    val path1: Path = Path ("/tmp/file1",'/')

    // when an absolute path is desired don't forget "/"
    val path2: Path = Path ("/","tmp","file1")

    // include windows examples for completeness
    val path3: Path = Path ("c:/tmp/file2",'/')
    val path4: Path = Path ("c:","tmp","file2")
    val path5: Path = Path ("""c:\tmp\file3""",'\\')

    // a filesystem can also be used to create Paths
    val path6: Path = FileSystem.default ("/tmp/file6",'/')
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
    val path1: Path = Path (new File ("/tmp/file1"))
    // include windows examples for completeness
    val path2: Path = Path (new File ("file://c:/tmp/file2"))
    val path3: Path = Path (new File ("file://c:\\tmp\\file3"))
  }

}
