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

    // Note: each write will open a new connection to file and each write
    //       is executed at the begining of the file so in this case the last write will
    //       be the contents of the file.
    // See Seekable for append and patching files
    // Also See openOutput for performing several writes with a single connection
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
    import JavaConverters.asOutputConverter

    // By default files can be converted to an Output Object by importing
    // Output.asOutputConverter and calling asOutput on the file
    val output:Output = new File("aFile").asOutput

    // needed for the write call below
    implicit val codec = Codec.UTF8

    output.write("data is being written to file")
  }

  /**
   * In order to perform several output operations with a single open
   * connection.
   */
  def multipleWritesSingleConnection {
    import scalax.io._
    import processing.Processor

    val output:Output = Resource.fromFile("someFile")

    // Output processor are used when one needs to perform batch writes on an output object
    // When a processor object is used a "processing" pipeline is created and the operations
    // are performed in batch form.
    // The following example will write 2 lines to the output object (a file in this case)
    // there are a few ways to use outputProcessors.  Following are a few patterns

    // This next example is the pattern most developers will likely be most comfortable with:
    for{
      // create a processor (signalling the start of a batch process)
      processor <- output.outputProcessor
      // create an output object from it
      out = processor.asOutput
    }{
      // all writes to out will be on the same open output stream/channel
      out.write("first write\n")
      out.write("second write")
    }

    // At first glance the next example seems odd
    // but it does the same as the last example
    for{
        // create the processor
        out <- output.outputProcessor
        // perform write calls
        // but realize that the writes do not occur until
        // the processor are executed (see next example for that)
        _ <- out.write("first write\n")
        _ <- out.write("second write")
    } {} // Nothing yields so the processor executes and the 2 lines are written

    // The next example creates a processing pipeline and then executes it
    val processor: Processor[Unit] = for{
        // create the processor
        out <- output.outputProcessor
        // perform write calls
        _ <- out.write("first write\n")
        _ <- out.write("second write")
    } yield {}
    // at this point the writes have not occurred because processor contains the
    // processing pipeline

    processor.execute  // execute processor

    // Why use the processor style writes? because it processors have both read and
    // write components.  Write processors allow reading and writing to be interleaved
    val processor2: Processor[TraversableOnce[Unit]] = for{
        in <- Resource.fromURLString("http://scala-lang.org").lines().processor
        out <- output.outputProcessor
        // This examples how the writes can be interleaved with reads, which is not possible if the output operations
        // did not return processors
        _ <- out.write("writing scala-lang\n")
        lineNum <- in.repeatUntilEmpty()
        _ <- out.write("lineNum: "+lineNum)
        next <- in.next
        _ <- out.write(next)
    } yield {}
    processor2.execute

  }
}
