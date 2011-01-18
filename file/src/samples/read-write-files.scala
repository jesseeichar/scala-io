/**
 * A Path are both Input and Output objects as well as Seekable so all read/write options available through those
 * are also possible directly on Path objects
 */
object ReadWriteFiles {


  def basicReadWrite {
    import scalax.file.Path
    implicit val codec = scalax.io.Codec.UTF8

    // Take the first set of non-empty lines, keeping the terminator for each line
    val nonEmptySpan = Path("file").lines(includeTerminator = true).
      dropWhile{_.isEmpty}.
      takeWhile{_.nonEmpty}

    // Write result from previous read to a new file
    Path("nonEmpty").writeStrings(nonEmptySpan)
  }
  /**
   * Safe way to read and write a file
   */
  def safeReadWrite{
    import scalax.file.{
      FileOps, Path, NotFileException}
    import java.io.FileNotFoundException
    import scala.util.control.Exception._
    // see codec examples in scala io core for details on why there is an implicit codec here
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path("file")
    val result:Either[Throwable,String] = catching (classOf[NotFileException],
                                          classOf[FileNotFoundException]) either { file.slurpString}

    result match {
      case Left(error) => println("oops not a file maybe a directory: "+error.getMessage)
      case Right(data) => println (data)
    }
  }

}
