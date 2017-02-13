/**
 * Examples of using the Resource API to wrap existing Java IO objects.
 * <p>
 * The Resource API can be used to adapt Java IO objects such as InputStreams and Channels.  The Resource object
 * provides several methods for wrapping common Java objects.  In the .Net implementation the Resource API would wrap
 * .Net IO objects.
 * </p>
 */
import resource.ExtractedEither
object Resources {

  /**
   * Wrap any closeable resources with a Resource.  The scala-arm resource.managed method can be used
   * to wrap many types of objects (like Closeable or jdbc connections) with the ARM (Automatic Resource Management) functionality
   * which handles closing the resource after use.
   * <p>
   * For common IO it is recommended to use a specific Resource.fromFoo methods so that the convenient
   * methods of bytes, chars, copy, etc... are available for use.  But for other things like jdbc connections
   * or RandomAccessFiles resource.managed is useful.
   * </p>
   */
  def wrapCloseables {
    import java.io.RandomAccessFile

    val fileResource = _root_.resource.managed(new RandomAccessFile("foo.txt","rw"))

    // now resource can be used using acquire etc... with the confidence that the resource will
    // be closed when done

    // acquireAndGet will make the request but any exceptions will be thrown when encountered
    val length:Long = fileResource.acquireAndGet(_.length)

    // If you want to handle the possible errors then acquireFor is what you want
    fileResource.acquireFor(_.readByte) match {
      case ExtractedEither(Left(errors)) =>
        errors.foreach {_.printStackTrace}
      case ExtractedEither(Right(b)) =>
        println("I got a byte :) "+b)
    }

  }
  
  /**
   * Create Resource from an Input Stream. Perform some conversions to obtain other types of Resources
   */
  def inputStreamResource {
    import java.io._
    import java.net.URL
    import java.nio.channels.ReadableByteChannel

    import scalax.io._
    import scalax.io.managed._

    val inputStream: InputStream = new URL("http://someurl.com").openStream

    // create a Resource object from an input stream.  It extends the Input Trait
    val in: InputStreamResource[InputStream] = Resource.fromInputStream(inputStream)
    // read a byte from input stream (although it is easier to use the Input API)
    val value = in acquireFor { _.read }

    // create a Resource object based on a ReadableByteChannel from the InputStreamResource
    // This extends Input Trait.
    // Why do this?  Some time one needs to send a ReadableByteChannel to a Java API
    // This way one can make the API call in a managed way.
    val readableChannel: Resource[ReadableByteChannel] = in.readableByteChannel
    // call a Legacy API in a managed way:
    object X {
      def toString(channel:ReadableByteChannel) = channel.toString
    }
    println(readableChannel acquireFor X.toString)

    // Similarly an InputStreamResource can be converted to a ReadCharsResource with an
    // underlying Reader object as the resource.  It should be noted that ReadCharsResource
    // extends the ReadChars Trait for reading lines, strings and characters
    val reader: ReadCharsResource [Reader] = in.reader
    // do something with reader like above
  }
  
  /**
   * Create Resource from an Output Stream. Perform some conversions to obtain other types of Resources
   */
  def outputStreamResource {
     import java.io._
     import java.nio.channels.WritableByteChannel

     import scalax.io._
     import scalax.io.managed._
    
    val outputStream: FileOutputStream = new FileOutputStream("file")
    
    // create a resource from a FileOutputStream.  The Resource extends Output Trait
    val out: OutputStreamResource[OutputStream] = Resource.fromOutputStream(outputStream)

    // Convert the output resource to a Resource based on a WritableByteChannel.  The Resource extends Output Trait
    val writableChannel: WritableByteChannelResource[WritableByteChannel] = out.writableByteChannel
    
    // Convert the output resource to a WriterResource which extends the WriteChars Trait and is based on a Writer
    val writer: WriterResource[Writer] = out.writer
  }
  /**
   * Create Resource from an files and RandomAccessFiles.  These channels extend 
   * Seekable Trait which is a subtrait of Input and Output Traits
   */
  def seekableByteChannelResource {
     import java.io._
     import java.nio.channels.FileChannel

     import scalax.io._
     import scalax.io.managed._

     // One way to create a SeekableByteChannel which is a Seekable object
     val channel: SeekableByteChannelResource[SeekableByteChannel] = Resource.fromFile("file")

     // A second way to create a SeekableByteChannel which is a Seekable object
     val channel2: SeekableByteChannelResource[SeekableByteChannel] =
         Resource.fromRandomAccessFile(new RandomAccessFile("file","rw"))

     // demonstrate that resources are Seekable and Input and Output objects
     val seekable: Seekable = channel2
     val inOut: Input with Output = channel

     // It is important to create the Resource correctly.  The following has an underlying Resource
     // of type FileChannel but it is not Seekable, it is just an Input and Output Subclass
     val channel3: ByteChannelResource[FileChannel] =
       Resource.fromByteChannel(new RandomAccessFile("file","rw").getChannel)
     val inOut2: Input with Output = channel2
     
     // ByteChannels also have conversion methods to outputStream, inputStream, 
     // reader, writer, etc...
     val outputStreamResource = channel3.outputStream
     channel2.writer
     channel.inputStream
     channel.readableByteChannel
   }

   /**
    * Several examples of creating Resources
    */
   def readableByteChannelResources {
     import java.io._
     import java.nio.channels.{Channels, ReadableByteChannel}

     import scalax.io._
     import scalax.io.managed._

     // see codec examples in scala io core for details on why there is an implicit codec here
     implicit val codec = scalax.io.Codec.UTF8

     val readableByteChannel = Channels.newChannel(new FileInputStream("file"))
     val readChannel : ReadableByteChannelResource[ReadableByteChannel] =
               Resource.fromReadableByteChannel(readableByteChannel)
     val in2:Input = readChannel
   }

  /**
   * Several examples of creating Resources
   */
  def writableByteChannelResources {
    import java.io._
    import java.nio.channels.{Channels, WritableByteChannel}

    import scalax.io._
    import scalax.io.managed._

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
    import java.io._
    import java.nio.channels._

    import scalax.io._
    import scalax.io.managed._

    val resource = Resource.fromInputStream(new FileInputStream("file"))

    // The Resource objects have methods for converting between the common types
    val readChars: ReadCharsResource[Reader] = resource.reader(Codec.UTF8)
    val readableByteChannel: ReadableByteChannelResource[ReadableByteChannel] =
              resource.readableByteChannel

    // there are also several ways to obtain the underlying java object
    // for certain operations.  Typically this is to micro manage how the
    // data is read from the input
    val availableBytes: Int = resource.acquireAndGet{
      inputStream => inputStream.available
    }

    // If you want to perform an operation and have the option to easily
    // get the exception acquireFor is a good solution
    val firstLine: Either[scala.List[scala.Throwable], String] = readChars.acquireFor{
      reader => new BufferedReader(reader).readLine()
    }
  }

  /**
   * Create Unmanaged resources.
   *
   * <p>It is not the common case but sometimes one wants to use the scala-io API on resources that
   * that should not be closed like Standard in/out/err.  Resource has factory methods for creating Resources
   * that will not be closed when an action is finished.  All methods that do not close the resource contain
   * the term Peristent.  For example fromUnmanagedStream</p>
   */
  def createResourcesThatArentClosed {
    import java.io.OutputStreamWriter

    import scalax.io.JavaConverters._
    import scalax.io.{Input, Output, WriteChars}

    val stdIn:Input = System.in.asUnmanagedInput
    val stdOut:Output = System.out.asUnmanagedOutput
    val stdErr:WriteChars = new OutputStreamWriter(System.err).asUnmanagedWriteChars

  }

  /**
   * Perform additional actions when a resource is closed. One of the important features of the Scala IO is that resources are cleaned up
   * automatically.  However occasionally one would like to perform an action on close in addition to
   * the default closing/flushing of the resource.  This can be done by updating the ResourceContext of
   * the Resource with a new CloseAction or multiple CloseActions
   */
  def performAdditionalActionOnClose {
    import scalax.io._

    // a close action can be created by passing a function to execute
    // to the Closer object's apply method
    // '''WARNING''' When defining a CloseAction make its type as generic
    // as possible.  IE if it can be a CloseAction[Closeable] do not
    // make it a CloseAction[InputStream].  The reason has to do
    // with contravariance.  If you don't know what that means
    // don't worry just trust me ;-)
    val closer = CloseAction{(channel:Any) =>
      println("About to close "+channel)
    }

    // another option is the extend/implement the CloseAction trait
    val closer2 = new CloseAction[Any]{

      protected def closeImpl(a: Any):List[Throwable] = {
        println("Message from second closer")
        Nil
      }
    }

    // closers can naturally be combined
    val closerThenCloser2 = closer +: closer2
    val closer2ThenCloser = closer :+ closer2

    // we can then create a resource and pass it to the closer parameter
    // now each time resource is used (and closed) the closer will also be executed
    // just before the actual closing.
    val resource = Resource.fromFile("file").addCloseAction(closer)
  }

  /**
   * This example examines why a CloseAction[Any] can be assigned
   * to a CloseAction[String] but not vice-versa.
   * <p>
   * Normally one think in terms of ''Covariance'' (List[String] can be assigned to a List[Any])
   * but that cannot work for CloseActions so CloseActions have the exact opposite characteristics.
   * </p>
   */
  def whyAreCloseActionsContravariant {
    import java.io._

    import scalax.io._

    // Since CloseAction is Defined as CloseAction[-A], the following compiles
    val action:CloseAction[String] = CloseAction[Any]{_ => ()}

    //But
    // val action:CloseAction[Any] = CloseAction[String]{_ => ()}
    // does not.

    // If you want to know why consider the following:
    val resource:Resource[InputStream] = Resource.fromInputStream(new FileInputStream("file"))
    val resource2:Resource[Closeable] = resource

    val closeAction:CloseAction[InputStream] = CloseAction{in:InputStream => println(in.available)}

    //Given the previous declarations it should be obvious that the following works
    val updatedResource:Resource[InputStream] = Resource.fromInputStream(new FileInputStream("file")).addCloseAction(closeAction)

    // However since resource2 is a Resource[Closeable] it should be obvious that one cannot
    // add a closeAction that requires an InputStream.  so the following would fail to compile
    // resource2.appendCloseAction(closeAction)
  }
}
