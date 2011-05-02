import java.io.File

/**
 * One of the core IO classes is called Output.  Normally the Output API will be encountered when a Resource object is
 * created.  But a resource is not required to have an Output object.
 */
object MoreOutputExamples {
  /**
   * All Output resources extend the Output trait so the following operations can be used on any Output resource
   * (ReadableByteChannel or OutputStream for example).
   * <p>
   * This example does not try to cover all operations since they are covered in multiple examples like in the
   * basic-read-write examples.
   * </p>
   */
  def basicOutput {
    import scalax.io._

    val output:Output = Resource.fromFile("someFile")

    output.writeIntsAsBytes(1,2,3)
    output.write("hello")(Codec.UTF8)
    output.writeStrings(List("hello","world")," ")(Codec.UTF8)
  }

  /**
   * In addition to Resource.fromFoo methods to create Resources (which are often Output objects)
   * There is a second option for converting certain objects directly to an Output object.  This example
   * shows how to convert a File to an output object
   */
  def convertObjectToOutput {
    import scalax.io._
    import java.io.File
    import Output.asOutputConverter

    // By default files can be converted to an Output Object by importing
    // Output.asOutputConverter and calling asOutput on the file
    val output:Output = new File("aFile").asOutput

    // needed for the write call below
    implicit val codec = Codec.UTF8

    output.write("data is being written to file")
  }

  /**
   *
   */
}
