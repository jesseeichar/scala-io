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
    import scalax.file.ImplicitConversions.string2path

    val filePath: Path = "/tmp/file"
  }

  /**
   * Implicitly convert files to paths
   */
  def javaFileToPath = {
    import java.io.File
    import scalax.file.Path
    import scalax.file.ImplicitConversions.jfile2path

    val filePath: Path = new File ("/tmp/file")
  }
}
