/**
 * One of the core IO classes is called Input.  Normally the Input API will be encountered when a Resource object is
 * created.  But a resource is not required to have an Input object.
 */
object InputExamples {
  /**
   * All Input resources extend the Input trait so the following operations can be used on any Input resource
   * (ReadableByteChannel or InputStream for example).
   * <p>
   * This example does not try to cover all operations since they are covered in multiple examples like in the
   * basic-read-write examples.
   * </p>
   */
  def basicInput {
    import scalax.io._

    val input:Input = Resource.fromFileString("someFile")

    // read all bytes into an in memory arry
    input.byteArray

    // skip first 5 bytes and take the next 5
    // force the operation to take place.
    // The bytes is a ResourceView which is a LongTraversableView,
    // meaning it will evaluate lazily until the data is forced
    // or requested
    input.bytes.drop(5).take(5).force

    // read all bytes into a string
    // note: codec can be passed implicitely as well
    input.slurpString(Codec.UTF8)
  }

  /**
   * Sometimes is can be handy to treat a List or Array of Bytes as an Input object.  This example
   * demonstrates how to do that
   */
  def convertTraversableToInput {
    import scalax.io._
    import Input.asInputConverter

    // any Traversable[Int] can be implicitly converted
    // to an Input by the implicit conversions in the
    // Input object
    val input1:Input = List[Int](1,2,3).asInput

    // needed for the chars call below
    implicit val codec = Codec.UTF8

    // all normal Input ops can be used on the list
    val chars = input1.chars mkString ","

    // Traversable[Byte] can also be converted to an Input
    val input2:Input = List[Byte](1,2,3).asInput

  }
}
