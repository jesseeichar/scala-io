/**
 * Examples demonstrating how to interoperate with Existing Java libraries.  The primary focus
 * is given a Java library that requires a InputStream, Reader, Writer, etc... what are some of
 * the strategies that can be used to obtain the underlying Java resource for the Java library.
 */
import scalax.io.managed.SeekableByteChannelResource
object JavaInterop {

  /**
   * demonstrate a few ways to interoperate existing java APIs
   */
  def basicInteropExamples {
    import scalax.io._
    import java.io._

    val file: SeekableByteChannelResource[SeekableByteChannel] =  Resource.fromFile(new File("file"))

    // some APIs require a stream or channel. Using one of the file resources you can safely call the method and be guaranteed that the stream will be correctly closed and exceptions handled
    // see the documentation in resource.ManagedResource for details on all the options available
    def javaApiEntryPoint(stream: InputStream) = {
      // do something interesting
      stream.read()
    }

    // here is the code for calling that method
    file.inputStream.acquireFor (javaApiEntryPoint)

    // other APIs use inversion of to obtain the file object.
    // here is how to get a raw java OutputStream from a file
    val out: OutputStream = file.outputStream.open().get

    // however the above pattern must be used with care since any close actions will not be executed.
    // If acquireFor cannot be used (for example if you are implementing an interator) you can do the following
    val openedResource = file.outputStream.open()
    // do something with resource
    val out2: OutputStream = openedResource.get
    // this will close out2 and execute any closeActions
    val errorsDuringClose: scala.List[scala.Throwable] = openedResource.close()
  }

}
