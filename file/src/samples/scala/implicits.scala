
/**
 * Using implicit conversions to convert between strings Java Files
 * and Scala Paths
 */
object implicits {
  /**
   * Implicitly convert strings to paths
   */
  def stringToFile = {
    import scalax.file.Path
    import scalax.file.ImplicitConversions._

    val filePath: Path = "/tmp/file"
  }

  /**
   * Implicitly convert files to paths
   */
  def javaFileToPath = {
    import java.io.File
    import scalax.file.Path
    import scalax.file.ImplicitConversions._

    val filePath: Path = new File ("/tmp/file")
  }

  /**
   * Implicitly convert files to paths
   */
  def pathTojavaFile = {
    import java.io.File
    import scalax.file.FileSystem
    import scalax.file.ImplicitConversions.defaultPath2jfile

    // DefaultPath objects can be converted to java.io.File objects
    val file: File = FileSystem.default("somefile")
  }

  /**
   * Examples of using the implicit converters to convert to
   * and from java.io.File and scalax.file.Path objects
   */
  def implicitConverters = {
    import java.io.File
    import java.nio.file.{Paths, Path => JPath}
    import scalax.file.Path
    import scalax.file.ImplicitConverters._

    val path: Path = new File("/tmp/file").asPath
    val path2: Path = Paths.get("/tmp/file").asPath
    val fileAgain: File = path.asJavaFile
    val javaPath: JPath = path.asJavaPath
    val pathFromString: Path = "hi".asPath
  }
}
