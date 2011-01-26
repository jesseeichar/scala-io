/**
 * Examples of using the Resource API to wrap existing Java IO objects.
 * <p>
 * The Resource API can be used to adapt Java IO objects such as InputStreams and Channels.  The Resource object
 * provides several methods for wrapping common Java objects.  In the .Net implementation the Resource API would wrap
 * .Net IO objects.
 * </p>
 */
object Resources {

  /**
   * Several examples of creating Resources
   */
  def createResources {
    import scalax.io._
    import java.io._
    import java.nio.channels._
    import java.net.URL

    // see codec examples in scala io core for details on why there is an implicit codec here
    implicit val codec = scalax.io.Codec.UTF8

    // get various input streams, readers an channels
    val inputStream: InputStream = new URL("http://someurl.com").openStream
    val in: InputStreamResource[InputStream] = Resource.fromInputStream(inputStream)
    val bufferedIn: InputStreamResource[BufferedInputStream] = in.buffered
    val readableChannel: Resource[ReadableByteChannel] = in.readableByteChannel
    val reader: ReaderResource[Reader] = in.reader
    val bufferedReader: ReaderResource[BufferedReader] = reader.buffered

    // get various output streams and channels
    val outputStream: FileOutputStream = new FileOutputStream("file")
    val out: OutputStreamResource[OutputStream] = Resource.fromOutputStream(outputStream)

    val bufferedOut: OutputStreamResource[BufferedOutputStream] = out.buffered
    val writableChannel: Resource[WritableByteChannel] = out.writableByteChannel
    val writer: WriterResource[Writer] = out.writer
    val bufferedWriter: WriterResource[BufferedWriter] = writer.buffered

    // examples getting ByteChannels
    // default is a read/write/create channel
    val channel: SeekableByteChannelResource[SeekableByteChannel] = Resource.fromFileString("file")
    val channel2: SeekableByteChannelResource[SeekableByteChannel] =
        Resource.fromRandomAccessFile(new RandomAccessFile("file","rw"))
    val seekable: Seekable = channel2
    val inOut: Input with Output = channel

    val channel3: ByteChannelResource[FileChannel] =
      Resource.fromByteChannel(new RandomAccessFile("file","rw").getChannel)
    val inOut2: Input with Output = channel2

    val readableByteChannel = Channels.newChannel(new FileInputStream("file"))
    val readChannel : ReadableByteChannelResource[ReadableByteChannel] =
              Resource.fromReadableByteChannel(readableByteChannel)
    val in2:Input = readChannel

    val writableByteChannel = Channels.newChannel(new FileOutputStream("file"))
    val writeChannel : WritableByteChannelResource[WritableByteChannel] =
              Resource.fromWritableByteChannel(writableByteChannel)
    val out2:Output = writeChannel
  }

  /**
   * The typical IO objects are java IO objects converted into Resource objects using the
   * Resource object's factory methods.  Once a Resource has been created there are methods for
   * converting between them.
   * <p>
   * The following examples demonstrate using the Resource objects and converting between them.
   * </p><p>
   * All resources are also Seekable, Input, Output, ReadChars and/or WriteChars so all normal IO operations are possible
   * but the following examples are Resource only operations</p>
   */
  def usingIoResources {
    import scalax.io._
    import java.io._
    import java.nio.channels._

    val resource = Resource.fromInputStream(new FileInputStream("file"))

    // The Resource objects have methods for converting between the common types
    val bufferedInput: InputStreamResource[BufferedInputStream] = resource.buffered
    val readChars: ReaderResource[Reader] = resource.reader(Codec.UTF8)
    val readableByteChannel: ReadableByteChannelResource[ReadableByteChannel] =
              resource.readableByteChannel
    val bufferedReader = readChars.buffered

    // there are also several ways to obtain the underlying java object
    // for certain operations.  Typically this is to micro manage how the
    // data is read from the input
    val availableBytes: Int = bufferedInput.acquireAndGet{
      bufferedInputStream => bufferedInputStream.available
    }

    // If you want to perform an operation and have the option to easily
    // get the exception acquireFor is a good solution
    val firstLine: Either[scala.List[scala.Throwable], String] = bufferedReader.acquireFor{
      reader => reader.readLine
    }
  }

  /**
   * Perform additional actions when a resource is closed. One of the important features of the Scala IO is that resources are cleaned up
   * automatically.  However occasionally one would like to perform an action on close in addition to
   * the default closing/flushing of the resource.  When a resource is created additional close actions can be added
   * and they will be executed just before the resource is closed.
   */
  def performAdditionalActionOnClose {
    import scalax.io._
    import nio.SeekableFileChannel

    // a close action can be created by passing a function to execute
    // to the Closer object's apply method
    val closer = CloseAction{(channel:SeekableFileChannel) =>
      println("About to close "+channel)
    }

    // another option is the extend/implement the CloseAction trait
    val closer2 = new CloseAction[SeekableFileChannel]{
      def closeImpl[U >: SeekableFileChannel](a: U) =
        println("Message from second closer")
    }

    // closers can naturally be combined
    val closerThenCloser2 = closer :: closer2
    val closer2ThenCloser = closer ++ closer2

    // we can then create a resource and pass it to the closer parameter
    // now each time resource is used (and closed) the closer will also be executed
    // just before the actual closing.
    val resource = Resource.fromFileString("file")(closer)

  }
}
