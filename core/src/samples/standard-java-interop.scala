object JavaInterop {

  { // demonstrate several ways to interoperate existing java APIs
    import scalax.io._
    import JavaConversions.asResource
    import java.io._

    val file: SeekableByteChannelResource[SeekableByteChannel] =  new File("file").asResource

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
    // and just for good measure it will be a BufferedOutputStream
    // streams and writer both have buffered versions, similar to their java counterparts
    val out: OutputStream = file.outputStream.buffered.open
  }

}
