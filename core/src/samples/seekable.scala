/**
 * Seekable permits random access read and write.
 * <div><strong>NOTE:</strong> insert positions depend on the type of data being inserted
 * <pre>insert(6,5L)</pre> will be insert 5L after the 6th Long(or 48th byte)
 *  in the Seekable
 * </div>
 */
object SeekableSamples {

  /**
   * Examples of patching a file.
   */
  def patch {
    import scalax.io._
    import java.io.File

    // see codec example for why codec is required
    implicit val codec = scalax.io.Codec.UTF8

    val file: Seekable =  Resource.fromFile(new File("file"))

    // write "people" after character 6
    // if the file is < 6 characters an underflow exception is thrown
    // if the patch extends past the end of the file then the file is extended
    // Note: If the offset is always dependent on the data being written
    file.patch(6, "people",OverwriteAll)
    file.patch(6, "people",OverwriteAll)(Codec.UTF8)

    // patch the file with a traversable of bytes
    // patch starts after 6th byte
    file.patch(6, "people".getBytes,OverwriteAll)

    // Overwrite only 2 bytes that are in the file.
    // the extra bytes will be inserted
    file.patch(2,List[Byte](1,2,3),OverwriteSome(2))
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

  /**
   * When writing, the unit of the position parameter is not always in bytes.
   * Rather it is determined by by what type of data is being written.
   * <div> For example if a string is being written then the position
   * refers to the position in characters.</div>
   */
  def positioningExamples {
    import scalax.io._
    // see codec example for why codec is required
    implicit val codec = Codec.UTF8

    val someFile: Seekable = Resource.fromFile("someFile")
    // "people" is being written after the 6th character
    someFile.patch(6, "people",OverwriteAll)

    // 1,2,3 is being written after the 8th byte (2nd integer)
    someFile.insert(2,List(1,2,3))

    // 1,2,3 is being written after the 2nd byte
    someFile.insert(2,List[Byte](1,2,3))
  }

  /**
   * Multiple Random accesses from a file opened only a single time
   */
  def multipleAccesses {
    // TODO at the moment I am not sure what the best way to perform multiple
    // read / write operations on a single file.  The typical seek - write - seek - read
    // does not seem very "scala-like"

    // A potential idea is to have the access like:
    // file.apply { file =>
    //   val name:Seekable = file.bytes.slice(5,10)  // take bytes 5 -> 10
    //   name.write("hello")(codec)  // overwrite bytes 5-10
    // }
  }
}
