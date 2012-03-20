package scalax.io
package processing

import java.io.OutputStream
import java.io.FilterOutputStream
import java.nio.channels.WritableByteChannel
import java.nio.ByteBuffer

/**
 * A processor that opens an Output Resource allowing for batch processing.  In other words it permits
 * multiple write operations to be performed on the same opened output.  The OpenOutput class
 * has essentially the same API as Output except that each method returns a Processor instead of
 * Unit.
 *
 * There is a important difference between returning a Processor or Unit.  Because OpenOutput returns
 * processor the writing is not done until the processor is executed.
 *
 * Examples:
 *
 * The following example is useful when one needs to do several output operations together without closing the output
 * object.
 *
 * {{{
 * for {
 *   outProcessor <- Resource.fromOutputStream(stream).outputProcessor
 *   out = outProcessor.asOutput
 * } {
 *   out.write("hi")
 *   out.write(123)
 * }
 * }}}
 *
 * This example is interesting because it illustrates that the methods in outProcessor are NOT the same
 * as in an Output object and to get the expected results writing within the for-comprehension body, the outputProcessor
 * must be converted to a normal output object and the write actions are performed on that.
 *
 * The following example shows how to read and write data from one input to an output
 *
 * {{{
 * val process = for {
 *   in1 <- inResource1.lines.processor
 *   in2 <- inResource2.lines.processor
 *   out <- Resource.fromFile("file").outputProcessor
 *   _ <- in1.repeatUntilEmpty(in2)
 *   line1 <- in1.nextOption
 *   line2 <- in2.nextOption
 *   line = line1 orElse line2
 *   _ <- out.write(line)
 *   _ <- out.write("\n")
 * } yield ()
 *
 * // At this point the process has not been executed, just defined
 * process.execute // execute process
 * }}}
 *
 * Since the methods in OpenOutput return Process object the methods can be called interleaved
 * within a process definition as follows:
 *
 * {{{
 * val process = for {
 *   in1 <- inResource1.lines.processor
 *   in2 <- inResource2.lines.processor
 *   out <- Resource.fromFile("file").outputProcessor
 *   _ <- in1.repeatUntilEmpty(in2)
 *   line1 <- in1.nextOption
 *   _ <- out.write(line1)
 *   line2 <- in2.nextOption
 *   _ <- out.write(line2)
 * } yield out.asOutput.write("\n")
 *
 * // At this point the process has not been executed, just defined
 * process.execute // execute process
 * }}}
 *
 * In this example notice that the writing in the for-comprehension body converts the outputProcessor to an Output
 * object before writing otherwise the write will not be executed.
 *
 * @param resource the resource to use in the process
 */
class OutputProcessor(resource: OutputResource[WritableByteChannel]) extends Processor[OpenOutput] {
  def context = resource.context

  def init = new Opened[OpenOutput] {
    val openedResource = resource.open()

    def execute() = Some(new OpenOutput(openedResource.get, context))

    def cleanUp() = openedResource.close()
  }
}

/**
 * The Processor API object for performing basic output operations within a processing pipeline.
 *
 * For examples on how to use see [[scalax.io.processing.OutputProcessor]]
 *
 * @see scalax.io.processing.OutputProcessor
 */
class OpenOutput private[processing](channel: WritableByteChannel, resourceContext:ResourceContext) {
  private val factory = new ProcessorFactory(resourceContext)
  val uncloseableChannel = new WritableByteChannel{
    def isOpen = channel.isOpen
    def write(src: ByteBuffer) = channel.write(src)
    override def close() {}
  }
  private[this] val out = new managed.WritableByteChannelResource[WritableByteChannel](uncloseableChannel, resourceContext, CloseAction.Noop)

  val asOutput:Output = out
  /**
   * Write data to the underlying object.  Each time write is called the resource is reopened, in the case of a
   * file this means that the file will be opened and truncated.  The
   *
   * In the case of writing ints and bytes it is often
   * recommended to write arrays of data since normally the underlying object can write arrays
   * of bytes or integers most efficiently.
   *
   * Since Characters require a codec to write to an OutputStream characters cannot be written with this method
   * unless a OutputWriterFunction.CharFunction object is provided as the writer.
   *
   * @see #writeChars for more on writing characters
   *
   * @param data
   *          The data to write to underlying object.  Any data that has a resolvable [[scalax.io.OutputConverter]] can
   *          be written.  See the [[scalax.io.OutputConverter]] object for the defined [[scalax.io.OutputConverter]]
   *          implementations and classes to assist implementing more.
   * @param writer
   *          The strategy used to write the data to the underlying object.  Many standard data-types are implicitly
   *          resolved and do not need to be supplied
   *
   * @return a unit processor
   */
  def write[T](data: T)(implicit writer: OutputConverter[T]): Processor[Unit] = {
    factory(Some(
        out.write(data)(writer)))
  }

  /**
   * Since the [[scalax.io.OutputConverter]] object defined for writing Ints encodes Ints using 4 bytes this method
   * is provided to simply write an array of Ints as if they are Bytes.  In other words just taking the first
   * byte.  This is pretty common in Java.io style IO.  IE
   *
   * {{{ outputStream.write(1) }}}
   *
   * 1 is written as a single byte.
   *
   * @return a unit processor
   */
  def writeIntsAsBytes(data: Int*): Processor[Unit] = {
    factory(Some(out.write(data)(OutputConverter.TraversableIntAsByteConverter)))
  }

  /**
   * Writes a string.
   *
   * @param string
   *          the data to write
   * @param codec
   *          the codec of the string to be written. The string will
   *          be converted to the encoding of { @link sourceCodec}
   *          Default is sourceCodec
   * @return a unit processor
   */
  def write(string: String)(implicit codec: Codec = Codec.default): Processor[Unit] = {
    factory(Some(
      out.write(string)(codec)))
  }

  /*
  * Writes Characters to the underlying object.
  *
  * @param characters the characters to write
  * @param codec the codec to use for encoding the characters
  *
  * @return a unit processor
  */
  def writeChars(characters: TraversableOnce[Char])(implicit codec: Codec = Codec.default): Processor[Unit] = {
    factory(Some(out.write(characters)(OutputConverter.charsToOutputFunction)))
  }

  /**
   * Write several strings.
   *
   * @param strings
   *          The data to write
   * @param separator
   *          A string to add between each string.
   *          It is not added to the before the first string
   *          or after the last.
   * @param codec
   *          The codec of the strings to be written. The strings will
   *          be converted to the encoding of { @link sourceCodec}
   * @return a unit processor
   */
  def writeStrings(strings: Traversable[String], separator: String = "")(implicit codec: Codec = Codec.default): Processor[Unit] = {
    factory(Some(out.writeStrings(strings, separator)(codec)))
  }

}