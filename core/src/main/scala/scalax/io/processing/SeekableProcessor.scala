package scalax.io
package processing

import Line.Terminators._
import java.nio.channels.Channels
import java.nio.ByteBuffer
import java.nio.channels.SeekableByteChannel

/**
 * A processor that opens an Output Resource allowing for batch processing.  In other words it permits
 * multiple write operations to be performed on the same opened output.  The OpenSeekable class
 * has essentially the same API as Seekable except that each method returns a Processor instead of
 * Unit.
 *
 * There is a important difference between returning a Processor or Unit.  Because OpenSeekable returns
 * processor the writing is not done until the processor is executed.
 *
 * The usage pattern of SeekableProcessor is the same as for [[scalax.io.processing.OutputProcessor]] so
 * look at that for more examples on how to use this class.
 *
 * {{{
 * val process = for {
 *   out <- Resource.fromFile("file").seekableProcessor
 *   _ <- out.write(line)
 *   _ <-
 *   _ <- out.write("\n")
 * } yield ()
 *
 * // At this point the process has not been executed, just defined
 * process.execute // execute process
 * }}}
 *
 *
 * @param resource the resource to use in the process
 *
 * @see scalax.io.processing.OutputProcessor
 * @see scalax.io.processing.OpenSeekable
 */
class SeekableProcessor(resourceOpener: => OpenedResource[SeekableByteChannel], val context:ResourceContext) extends Processor[OpenSeekable] {
  def init = new Opened[OpenSeekable] {
    val openedResource = resourceOpener

    def execute() = Some(new OpenSeekable(openedResource.get, context))

    def cleanUp() = openedResource.close()
  }
}

/**
 * The Processor API object for performing seekable operations within a processing pipeline.
 *
 * This is an extension of [[scalax.io.processing.OpenOutput]] and that class should be viewed for
 * details on how to use this class.  In fact an even better resource is [[scalax.io.processing.OutputProcessor]]
 * since it has good examples.
 *
 * @see scalax.io.processing.OpenOutput
 * @see scalax.io.processing.OutputProcessor
 *
 * @define overwriteParam @param overwrite The strategy that dictates how many characters/bytes/units are overwritten
 * @define outputConverter [[scalax.io.OutputConverter]]
 * @define dataParam  @param data
 *          The data to write.  This can be any type that has a $outputConverter associated
 *          with it.  There are predefined $outputConverters for several types.  See the
 * $outputConverter object for the predefined types and for objects to simplify implementing
 *          custom $outputConverter
 * @define intAsByteExplanation Since the [[scalax.io.OutputConverter]] object defined for writing Ints encodes Ints using 4 bytes this method
 *  is provided to simply write an array of Ints as if they are Bytes.  In other words just taking the first
 *  byte.  This is pretty common in Java.io style IO.  IE
 * {{{ outputStream.write(1) }}}
 *  1 is written as a single byte.
 * @define arrayRecommendation '''Important:''' The use of an Array is highly recommended
 *  because normally arrays can be more efficiently written using
 *  the underlying APIs
 * @define patchDesc Update a portion of the file content at
 *  the declared location. This is the most flexible of the
 *  random access methods but is also (probably) the trickiest
 *  to fully understand.  That said it behaves (almost) identical
 *  to a scala.collection.Seq.patch method, so if you understand that
 *  you should not have difficulty understanding this method.
 * @define converterParamconverterParam @param converter The strategy for writing the data/converting the data to bytes
 */
class OpenSeekable private[processing] (channel: SeekableByteChannel, resourceContext: ResourceContext) extends OpenOutput(channel, resourceContext) {
  private[this] val factory = new ProcessorFactory(resourceContext)
  private[this] val nonCloseable = new SeekableByteChannel with Adapter[SeekableByteChannel] {
    def src = channel
    def close() {}
    def isOpen: Boolean = true
    def write(src: ByteBuffer): Int = channel.write(src)

    def truncate(size: Long): SeekableByteChannel = channel.truncate(size)

    def size: Long = channel.size

    def read(dst: ByteBuffer): Int = channel.read(dst)

    def position(newPosition: Long): SeekableByteChannel = channel.position(newPosition)

    def position: Long = channel.position
  }

  val asSeekable: Seekable {
    def position: Long
    def position_=(newPosition: Long): Unit
  } = {
    val newContext = resourceContext.copy(newDescName = Some(KnownName("Seekable opened resource")))
    val sizeFunc = () => Some(channel.size)
    new managed.SeekableByteChannelResource[SeekableByteChannel](_ => nonCloseable, newContext, CloseAction.Noop, sizeFunc, None) {
      def position: Long = channel.position
      def position_=(newPosition: Long): Unit = channel.position(newPosition)

    }
  }

  /**
   * $patchDesc
   *
   * If the position is beyond the end of the file a BufferUnderflow
   * Exception will be thrown
   *
   * If the position is within the file but the
   * `position + string.getBytes(codec).length`
   * is beyond the end of the file the file will be enlarged so
   * that the entire string can fit in the file
   *
   * The write begins at the position indicated.  So if position = 0
   * then the write will begin at the first byte of the file.
   *
   * @param position
   *          The start position of the update starting at 0.
   *          The position is the position'th character in the file using
   *          the codec for decoding the file
   *          The position must be within the file.
   * @param string
   *          The string to write to the file starting at
   *          position.
   * $overwriteParam
   * @param codec
   *          The codec to use for decoding the underlying data into characters
   */
  def patch(position: Long,
    string: String,
    overwrite: Overwrite)(implicit codec: Codec = Codec.default): Processor[Unit] = {
    factory(Some(asSeekable.patch(position, string, overwrite)(codec)))
  }

  /**
   * $patchDesc
   *
   * $arrayRecommendation
   *
   * To append data the `position must >= size`
   *
   * If the position is within the file but the
   * `position + bytes.length`
   * is beyond the end of the file the file will be enlarged so
   * that the entire string can fit in the file
   *
   * The write begins at the position indicated.  So if position = 0
   * then the write will begin at the first byte of the file.
   *
   * @param position
   *          The start position of the update starting at 0.
   *          The position must be within the file or == size (for appending)
   * $dataParam
   * $overwriteParam
   * $converterParam
   */
  def patch[T](position: Long,
    data: T,
    overwrite: Overwrite)(implicit converter: OutputConverter[T]): Processor[Unit] = {
    factory(Some(asSeekable.patch(position, data, overwrite)(converter)))
  }

  /**
   * $intAsByteExplanation
   */
  def patchIntsAsBytes(position: Long,
    overwrite: Overwrite,
    data: Int*): Processor[Unit] = {
    patch(position, data, overwrite)(OutputConverter.TraversableIntAsByteConverter)
  }

  /**
   * Inserts a string at a position in the Seekable. This is a potentially inefficient because of the need to
   * count characters.  If the codec is not a fixed sized codec (for example UTF8) each character must be
   * converted in the file up to the point of insertion.
   *
   * @param position The position in the file to perform the insert.  A position of 2 will insert the character after
   *              the second character (not byte).
   * @param string The string that will be inserted into the Seekable
   * @param codec The codec to use for determining the location for inserting the string and for encoding the
   *              string as bytes
   */
  def insert(position: Long, string: String)(implicit codec: Codec = Codec.default): Processor[Unit] = {
    insert(position, codec encode string)
  }

  /**
   * Inserts data at a position in the Seekable.  The actual position in the Seekable where the data is inserted depends on
   * the type of data being written.  For example if Longs are being written then position calculated as position * 8
   *
   * $arrayRecommendation
   *
   * @param position  The position where the data is inserted into the Seekable.  The actual position in the Seekable
   * where the data is inserted depends on the type of data being written.  For example if
   * Longs are being written then position calculated as position * 8
   *
   * $dataParam
   *
   * $converterParam
   */
  def insert[T](position: Long, data: T)(implicit converter: OutputConverter[T]): Processor[Unit] = {
    factory(Some(asSeekable.insert(position, data)(converter)))
  }

  /**
   * $intAsByteExplanation
   */
  def insertIntsAsBytes(position: Long, data: Int*) = insert(position, data)(OutputConverter.TraversableIntAsByteConverter)

  /**
   * Append bytes to the end of a file
   *
   * $arrayRecommendation
   *
   * $dataParam
   *
   * $converterParam
   */
  def append[T](data: T)(implicit converter: OutputConverter[T]): Processor[Unit] = {
    factory(Some(asSeekable.append(data)(converter)))
  }

  /**
   * $intAsByteExplanation
   */
  def appendIntsAsBytes(data: Int*): Processor[Unit] = {
    append(data)(OutputConverter.TraversableIntAsByteConverter)
  }

  /**
   * Append a string to the end of the Seekable object.
   *
   * @param string
   *          the data to write
   * @param codec
   *          the codec of the string to be written. The string will
   *          be converted to the encoding of { @link codec}
   */
  def append(string: String)(implicit codec: Codec = Codec.default): Processor[Unit] = {
    append(codec encode string)
  }

  /**
   * Append several strings to the end of the Seekable object.
   *
   * @param strings
   *          The strings to write
   * @param separator
   *          A string to add between each string.
   *          It is not added to the before the first string
   *          or after the last.
   * @param codec
   *          The codec of the strings to be written. The strings will
   *          be converted to the encoding of { @link codec}
   */
  def appendStrings(strings: Traversable[String], separator: String = "")(implicit codec: Codec = Codec.default): Processor[Unit] = {
    factory(Some(asSeekable.appendStrings(strings, separator)(codec)))
  }

  /**
   * Truncate/Chop the Seekable to the number of bytes declared by the position param
   */
  def truncate(position: Long): Processor[Unit] = {
    factory(Some(asSeekable.truncate(position)))
  }

  def truncateString(position: Long)(implicit codec: Codec = Codec.default): Processor[Unit] = {
    factory(Some(asSeekable.truncateString(position)(codec)))
  }

  def position: Processor[Long] = factory(Some(channel.position))

  def position_=(newPosition: Long): Processor[Unit] = factory(Some(channel.position(newPosition)))

  def size = factory(Some(channel.size))

  /**
   * Obtains a Traversable for conveniently processing the resource as bytes.
   *
   * @return
   */
  def bytes = asSeekable.bytes.processor

  /**
   * Read the input as blocks of bytes.  This method should be avoided unless the maximum performance is
   * absolutely required because bytes provides very good performance and is a better API for most applications.
   *
   * However since it better reflects how data is read with most input sources (like InputStreams and ReadableByteChannels);
   * blocks is slightly more performance than bytes and therefore
   * can be used when performance is the most important consideration.
   *
   * @param blockSize block size can optionally be specified but the default is normally acceptable.
   */
  def blocks(blockSize: Option[Int] = None) = asSeekable.blocks(blockSize).processor

  /**
   * Obtains a Traversable for conveniently processing the file as Ints.
   *
   * @return an non-strict traversable over all the bytes with the bytes being represented as Ints
   */
  def bytesAsInts = asSeekable.bytesAsInts.processor

  /**
   * This method aspires to be the fastest way to read
   * a stream of known length into memory.
   */
  def byteArray: Processor[Array[Byte]] = factory(Some(asSeekable.byteArray))

  /**
   * The characters in the object.
   *
   * If the codec is not the same as the source codec (the codec of
   * the underlying data) then the characters will converted to the
   * desired codec.
   *
   * @param codec
   *          The codec representing the desired encoding of the characters
   * @return
   *          an traversable of all the characters
   */
  def chars(implicit codec: Codec = Codec.default) = asSeekable.chars.processor

  /**
   * Obtain an non-strict traversable for iterating through the lines in the object
   *
   * If the codec is not the same as the source codec (the codec of
   * the underlying data) then the characters will converted to the
   * desired codec.
   *
   * @param codec
   *          The codec representing the desired encoding of the characters
   * @param terminator
   *          The strategy for determining the end of line
   *          Default is to auto-detect the EOL
   * @param includeTerminator
   *          if true then the line will end with the line terminator
   *          Default is false
   *
   * @return
   *          a non-strict traversable for iterating through all the lines
   */
  def lines(terminator: Terminator = Auto,
    includeTerminator: Boolean = false)(implicit codec: Codec = Codec.default) = {
    asSeekable.lines(terminator, includeTerminator)(codec).processor
  }
  /**
   * Loads all the characters into memory. There is no protection against
   * loading very large files/amounts of data.
   *
   * @param codec
   *          The codec representing the desired encoding of the characters
   */
  def string(implicit codec: Codec = Codec.default) = factory(Some(asSeekable.string(codec)))
}
