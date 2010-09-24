object implicits {
  def string = { // implicitly convert strings to paths
    import scalax.io.Path
    import Path.string2path

    val filePath: Path = "/tmp/file"
  }

  def jFile = { // implicitly convert files to paths
    import java.io.File
    import scalax.io.Path
    import Path.jfile2path

    val filePath: Path = new File ("/tmp/file")
    sdfsdf
  }
}