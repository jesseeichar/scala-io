package scalax.io
package processing

import java.io.{Writer, OutputStream, FilterOutputStream}


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
class WriteCharsProcessor(resource: Resource[Writer]) extends Processor[OpenWriteChars] {
  def context = resource.context

  def init = new Opened[OpenWriteChars] {
    val openedResource = resource.open()

    def execute() = Some(new OpenWriteChars(openedResource.get, context))

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
class OpenWriteChars private[processing](writer: Writer, resourceContext:ResourceContext) {
  self =>
  private val factory = new ProcessorFactory(resourceContext)
  private[this] val out = new managed.WriterResource[Writer](null, resourceContext, CloseAction.Noop) {
        override def open():OpenedResource[Writer] = new UnmanagedOpenedResource(new Writer {
          override def write(c: Int) = self.writer.write(c)
          override def write(cbuf: Array[Char]) = self.writer.write(cbuf)
          override def write(str: String) = self.writer.write(str)
          override def write(str: String, off: Int, len: Int) = self.writer.write(str, off, len)
          override def append(csq: CharSequence) = self.writer.append(csq)
          override def append(csq: CharSequence, start: Int, end: Int) = self.writer.append(csq,start,end)
          override def append(c: Char) = self.writer.append(c)
          def write(cbuf: Array[Char], off: Int, len: Int) = self.writer.write(cbuf, off, len)
          def flush() = self.writer.flush()
          override def close() = ()
          }, resourceContext)
  }

  val asWriteChars: WriteChars = out

  /**
   * Write several characters to the underlying object
   */
  def write(characters: TraversableOnce[Char]): Processor[Unit] =
    factory(Some(out.write(characters)))

  /**
   * Write several strings. The open options that can be used are dependent
   * on the implementation and implementors should clearly document
   * which option are permitted.
   *
   * @param strings
   *          The data to write
   * @param separator
   *          A string to add between each string.
   *          It is not added to the before the first string
   *          or after the last.
   */
  def writeStrings(strings: Traversable[String], separator: String = ""): Processor[Unit] =
    factory(Some(out.writeStrings(strings, separator)))
}
