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
    someFile.appendStrings(List("s one", "s two"),Line.Terminators.RNPair.sep)
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
   * In addition to Resource.fromFoo methods to create Resources (which are often Seekable objects)  
   * There is a second option for converting certain objects directly to an Seekable object.  This example
   * shows how to convert a File to an output object
   */
  def convertObjectToSeekable {
    import scalax.io._
    import java.io.File
    import Seekable.asSeekableConverter

    // By default files can be converted to an Seekable Object by importing
    // Seekable.asSeekableConverter and calling asSeekable on the file
    val seekable:Seekable = new File("aFile").asSeekable

    // needed for the append and slurp calls below
    implicit val codec = Codec.UTF8

    seekable.append("data is being written to file")
    val data:String = seekable.slurpString
  }
  /**
   * Multiple Random accesses from a file opened only a single time
   */
  def multipleAccesses {
    import scalax.io._

    val someFile: Seekable = Resource.fromFile("someFile")

    // Often it is preferable to open a file/resource a single time
    // and perform several operations on that file with the single
    // connection as it normally offers better performance and uses
    // fewer resources

    // The following will replace the first instanceof "hello" with "Hello"
    someFile.open{ file =>
      val index = file.chars.indexOf("hello")
      file.patch(index,"Hello",OverwriteAll)
    }

    // This example demonstrates processing a file as strings and
    // while convenient if the codec used is a variable sized codec like
    // UTF8 it can be quite expensive since determining the index in the file
    // requires counting from the beginning of the file each time
    someFile.open{ file =>
      val index = file.chars.indexOf("hello")
      // move to position index
      file.position = index
      // overwrite data starting at index with Hello
      file.write("Hello")
      // continue overwriting with World.
      // It is important to note that we are not inserting data
      // data insertion is intentionally made more difficult because
      // it is very expensive since copies of the file must be made
      // to perform the operation
      file.write(" World")
      file.insert(index,"<")

      file.insert(index+("Hello World".size),">")
    }

  }
}
