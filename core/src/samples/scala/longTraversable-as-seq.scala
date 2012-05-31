/**
 * Since a input source is typically a sequence of bytes (file, inputstream etc...) it is tempting
 * to treat them as a scala Seq or Iterable.  However the scala Seq and Iterable classes have methods
 * that are either difficult to implement on a stream of unknown size or are simply dangerous since they
 * can result in resource leaking.  In particular iterator is extremely likely to result in leaked
 * resources.
 * <p>
 * However there are several methods that not in the typical scala Traversable trait which logically
 * apply to a input source.  The following examples demonstrate several such methods.
 * </p>
 */
object LongTraversableAsSeq {
  /**
   * The iterable methods of zip, zipAll and zipWithIndex are natural fits for
   * combining input sources and other sequences such as scala Streams
   */
  def zipping {
    import scalax.io._

    val file1 = Resource.fromFile("file1")
    val file2 = Resource.fromFile("file2")

    // A simple example of comparing all bytes in
    // one file with those of another.
    // combining zip with sliding is a good way to perform operations
    // on sections of two (or more) files
    file1.bytes.zip(file2.bytes) map {
      case (file1Byte, file2Byte) =>
        file2Byte < file1Byte
    }

    //
    // Add a line number to each line in a file
    //
    // Note:  Since methods in a Input object return LongTraversableView objects
    // all zip examples do not open the file.  To do that you must call
    // force or some other method that forces a read to take place.
    //
    //
    val addedLineNumbers = file1.lines().zipWithIndex.map {
      case (line,idx) => idx+" "+line
    }
  }

  /**
   * Examples of a grab bag of useful Seq methods.
   */
  def grabBagOfExamples {
    import scalax.io._

    val file1 = Resource.fromFile("file1")
    val file2 = Resource.fromFile("file2")

    // obtain fourth byte.  In this case a FileChannel is opened, the index
    // is moved to the third byte and one byte is read making the method
    // relatively efficient (notice the word relatively ;-) )
    val fourthByte = file1.bytes(3)

    val firstIndexOfB = file1.chars.indexOf('B')

    // Find the index of the last line with more than thirty characters
    // Since the line is read from the file it might be more useful to
    // perform use find to actually find the line, but who knows what the usecase is
    val greaterThan30Characters = file1.lines().lastIndexWhere(_.size > 30)

    // check if file 1 startsWith file 2
    file1.bytes.startsWith(file2.bytes)

    // The number of consecutive lines starting at 0 containing e
    file1.lines().segmentLength(_ contains "e",0)

    // check if all lines in file1 are the same as in file2 ignoring case
    file1.lines().corresponds(file2.lines())(_ equalsIgnoreCase _)
  }

  /**
   * Similar to sameElements in Seq or Iterable, sameElements checks each
   */
  def sameElements {
    import scalax.io._

    val file1 = Resource.fromFile("file1")
    val file2 = Resource.fromFile("file2")

    // Check if file1has the same bytes as file2
    file1.bytes.sameElements(file2.bytes)

    // silly example but shows that the compared against
    // value can be an arbitrary Iterable
    file1.bytes.sameElements(1 to 30)
  }

  /**
   * Create a sliding 'window' on the underlying data.
   * The sliding method is part of the LongTraversable in order
   * to permit easy processing of the data in chunks rather
   * than one byte at a time.  An alternative would be to use foldLeft
   * and manage the buffer of data manually but sliding reduces the complexity of
   * performing such operations.
   * <p>
   * For example, suppose every 1000 bytes is a logical block and each block has a
   * checksum of 8 bytes at the end of the 1000 bytes.  The following would provide
   * one method of processing such a file
   * </p>
   */
  def slidingOnLongTraversable {
    import scalax.io._

    val file1 = Resource.fromFile("file1")

    // use sliding to visit each 1008 bytes.
    // map splits the window into two parts, block and checksum
    // NOTE:  Data is not loaded from the resource until foreach
    // or some other method that triggers the actual access of the data
    val blocks = file1.bytes.sliding(1008,1008).map{_ splitAt 1000}

    // grouped is sliding(size,size) so the following is equivalent
    val blocks2 = file1.bytes.grouped(1008).map{_ splitAt 1000}

    blocks foreach {
      case (block,checksum) =>
        // verify checksum and process
    }
  }
}
