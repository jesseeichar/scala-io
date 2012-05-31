/**
 * Examples for reading sections of an Input object without reading all data in the
 * underlying resource.  IE skip bytes and prematurely close resource.
 */
object AccessSegmentOfInput {

  /**
   * How to skip the first X bytes.
   */
  def skippingBytes {
    import scalax.io._

    val in:Input = Resource.fromURL("file://someFile")

    // Skip the first 10 bytes.  If underlying resource supports it the
    // bytes will not be read
    // Remember resource is lazy as of assignment resource has not yet been opened
    val restOfBytes = in.bytes.drop(10)

    // After assignment of ''string'' the resource has been opened and processed
    val string = new String(restOfBytes.toArray)
  }

  /**
   * How to close resource after reading first X bytes
   */
  def takingBytes {
    import scalax.io._

    val in:Input = Resource.fromURL("file://someFile")

    // Take first 10 bytes then close resource.
    // Remember the resource is lazy so firstTenBytes is not
    // an in-memory collection and accessing it will open
    // the resource and read the data.  At the moment of
    // ''firstTenBytes'' the resource has not been opened.
    val firstTen = in.bytesAsInts.take(10)

    // This statement will open resource take first 10 bytes add
    // them together and close resource.
    val sum = firstTen.reduceLeft(_ + _)
  }

  /**
   * How to take bytes until a condition is met and then close resource
   */
  def takeWhile {

    import scalax.io._

    val in:Input = Resource.fromURL("file://someFile")

    // take bytes until they one is greater than 5
    // Note: bytes is a view which means it does not open the
    // resource until the actual data is requested.
    // The following statement does not access resource
    val lessThan5 = in.bytesAsInts.takeWhile(_ < 5)


    // This statement will open resource take add
    // bytes together (until a byte greater than 5)
    // and close resource.
    val sum = lessThan5.reduceLeft(_ + _)
  }

  /**
   * Demonstrate the limited/controlled fold of LongTraversable.
   * <p>
   * The limitFold method allows one to fold over a LongTraversable but
   * terminate the traversal at any arbitrary point.  This is not as powerful
   * as the Iteree pattern but is easy to get into if one is familiar with fold
   * </p>
   */
  def limitedFold {

    import scalax.io._

    val in:Input = Resource.fromURL("file://someFile")

    // add bytes until the fifth encounter of a 5 occurs
    val (fives, sum) = in.bytes.limitFold((0,0)) {
      // By returning End the traversal will stop and the resource closed
      case ((5,sum), 5) => End((5, sum))
      // by returning Continue the traversal will continue
      case ((fives,sum), 5) => Continue((fives + 1, sum + 5))
      // by returning Continue the traversal will continue
      case ((fives,sum), next) => Continue((fives, sum + next))
    }
  }

  /**
   * An example with drop take and limitFold.
   */
  def puttingItTogether {

    import scalax.io._

    val in:Input = Resource.fromURL("file://someFile")

    /**
     * Skip first 10 bytes and sum a random number of bytes up
     * to 20 bytes
     */
    in.bytes.drop(10).take(20).limitFold(10) {
      case (acc, next) if util.Random.nextBoolean => End(acc + next)
      case (acc, next) => Continue(acc + next)
    }
  }

}
