
object SeekableSamples {

  /**
   * examples of patching a file
   */
  def patch {
    import scalax.io._
    import java.io.File

    // see codec example for why codec is required
    implicit val codec = scalax.io.Codec.UTF8

    val file: Seekable =  Resource.fromFile(new File("file"))

    // write "people" at byte 6
    // if the file is < 6 bytes an underflow exception is thrown
    // if the patch extends past the end of the file then the file is extended
    file.patch(6, "people",OverwriteAll)
    file.patch(6, "people",OverwriteAll)(Codec.UTF8)

    // patch the file with a traversable of bytes
    file.patch(6, "people".getBytes,OverwriteAll)

    file.patch(2,List(1,2,3),OverwriteSome(2))
  }

  /**
   * Inserts data anywhere in the file/seekable object
   */
  def insert {
    import scalax.io._

    // see codec example for why codec is required
    implicit val codec = Codec.UTF8

    val someFile: Seekable = Resource.fromFile("someFile")
    someFile.insert(3,List[Byte](3,2,1))
  }

  /**
   * examples of appending data to the end of a Seekable (In this case files)
   */
  def append {
    import scalax.io._

    // see codec example for why codec is required
    implicit val codec = Codec.UTF8

    val someFile: Seekable = Resource.fromFile("someFile")
    someFile.append("append this string")
    someFile.appendStrings(List("s one", "s two"),Line.Terminators.Pair.sep)
  }

}
