/**
 * A Path are both Input and Output objects as well as Seekable so all read/write options available through those
 * are also possible directly on Path objects
 */
object ReadWriteFiles {

  /**
   * Basic read and write options.  Not all options are demonstrated so review
   * the {/core} operations for Input,Output,Seekable etc....
   */
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
   * Safe way to read and write a file.  Normal try-catch will also work,
   * scala.util.control.Exception is nice when used with an API that takes Either or
   * Option.
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
  /**
   * Demonstrate ways to control how the a path is opened for write.
   * <p>Typically a write will overwrite an existing file. The seekable interface
   * can be used for some control (patch, insert append) but there are more options
   * like failing to write if file exists or open in append mode, etc...</p>
   */
  def controlledPathOpen {
    import scalax.file.Path
    import scalax.io.StandardOpenOption._
    // open the file for appending,  it will fail if file does not exist and
    // it will delete the file after editing the file

    val processor = for{
      processor <- Path("someFile").seekableProcessor(Seq(DeleteOnClose))
      // any number of Seekable actions can be performed
      _ <- processor.append(" an ending")
      chars <- processor.chars
      subSection <- chars.takeWhile(_ != '.')
    } yield subSection

   val firstSentence:Option[Seq[Char]] = processor.acquireAndGet(a => a)


    Path("someFile").outputStream(WriteAppend:_*).write("appending")
    Path("someFile").outputStream(WriteTruncate:_*).write("replace all data with this")
    Path("someFile").outputStream(Write).write("just replace beginning of file with this")
  }
}
