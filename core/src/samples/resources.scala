object Resources {

  def createResources { // several examples of obtaining Resources
    import java.nio.channels._
    import scalax.io._
    import java.io._
    import java.nio.channels._
    import java.net.URL

    // see codec examples in scala io core for details on why there is an implicit codec here
    implicit val codec = scalax.io.Codec.UTF8

    // get various input streams, readers an channels
    val in: InputStreamResource[InputStream] = Resource.fromInputStream(new URL("http://someurl.com").openStream)
    val bufferedIn: InputStreamResource[BufferedInputStream] = in.buffered
    val readableChannel: Resource[ReadableByteChannel] = in.readableByteChannel
    val reader: ReaderResource[Reader] = in.reader
    val bufferedReader: ReaderResource[BufferedReader] = reader.buffered

    // get various output streams and channels
    val out: OutputStreamResource[OutputStream] = Resource.fromOutputStream(new FileOutputStream("file"))

    val bufferedOut: OutputStreamResource[BufferedOutputStream] = out.buffered
    val writableChannel: Resource[WritableByteChannel] = out.writableByteChannel
    val writer: WriterResource[Writer] = out.writer
    val bufferedWriter: WriterResource[BufferedWriter] = writer.buffered

    // examples getting ByteChannels
    // default is a read/write/create channel
    val channel: SeekableByteChannelResource[SeekableByteChannel] = Resource.fromFile("file")
    val channel2: SeekableByteChannelResource[SeekableByteChannel] = Resource.fromRandomAccessFile(new RandomAccessFile("file","rw"))
    val seekable: Seekable = channel2
    val inOut: Input with Output = channel

    val channel3: ByteChannelResource[FileChannel] = Resource.fromByteChannel(new RandomAccessFile("file","rw").getChannel)
    val inOut2: Input with Output = channel2

    val readChannel = Resource.fromReadableByteChannel(Channels.newChannel(new FileInputStream("file")))
    val in2:Input = readChannel

    val writeChannel = Resource.fromWritableByteChannel(Channels.newChannel(new FileOutputStream("file")))
    val out2:Output = writeChannel
  }

  /**
   * The typical IO objects are java IO objects converted into Resource objects using the
   * Resource object's factory methods.  Once a Resource has been created there are methods for
   * converting between them.
   *
   * The following examples demonstrate using the Resource objects and converting between them.
   *
   * All resources are also Seekable, Input, Output, ReadChars and/or WriteChars so all normal IO operations are possible
   * but the following examples are Resource only operations
   */
  def usingIoResources {
    import scalax.io._
    import java.io._
    import java.nio.channels._

    val resource = Resource.fromInputStream(new FileInputStream("file"))

    // The Resource objects have methods for converting between the common types
    val bufferedInput: InputStreamResource[BufferedInputStream] = resource.buffered
    val readChars: ReaderResource[Reader] = resource.reader(Codec.UTF8)
    val readableByteChannel: ReadableByteChannelResource[ReadableByteChannel] = resource.readableByteChannel
    val bufferedReader = readChars.buffered

    // there are also several ways to obtain the underlying java object for certain operations
    // Typically this is to micro manage how the data is read from the input
    val availableBytes: Int = bufferedInput.acquireAndGet{bufferedInputStream => bufferedInputStream.available}

    // If you want to perform an operation and have the option to easily get the exception acquireFor is a good solution
    val firstLine: Either[scala.List[scala.Throwable], String] = bufferedReader.acquireFor{reader => reader.readLine}
  }

}
