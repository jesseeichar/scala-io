import java.io.StringWriter

/**
 * Examples for creating Output/Input/ReadChars/WriteChars/etc using the asFooConverter pattern.
 * 
 * There are two patterns for creating IO objects.  One is using the Resources API.  This is often the best for
 * creating resources from closeable objects such as InputStreams.  The Resource API takes a code block for constructing
 * the resource and can there for recreate the resource when needed.
 * <br/>
 * The other pattern is to convert an existing object to an Input/Ouput/ReadChars/WriteChars/Seekable object.  The
 * idea here is to import the implicit conversions contained in the target object (Input/Ouput/ReadChars/WriteChars/Seekable)
 * and then call the asInput/asOuput/asReadChars/asWriteChars/asSeekable.
 * <br/>
 * Examples of the latter pattern are described here.
 * <br/>
 * Note: In all examples where a codec is required, the codec can be explicitly or implicitly passed
 */
object AsFooConverter {
  /**
   * Convert to Input
   */
  def asInput {
    import scalax.io.Codec
    import scalax.io.JavaConverters._
    
    val webpage:String = new java.net.URL("http://www.scala-lang.org").asInput.slurpString(Codec.UTF8)
    val bytes:Array[Byte] = List[Byte](1,2,3,4).asInput.byteArray
  }
  /**
   * Convert to Output
   */
  def asOutput {
    import scalax.io.Codec
    import scalax.io.JavaConverters._

    implicit val codec = Codec.UTF8

    new java.io.File("io").asOutput.write("hi file")
  }
  /**
   * Convert to Seekable
   */
  def asSeekable {
    import scalax.io.JavaConverters._
    
    new java.io.File("io").asSeekable.insert(2,List[Byte](1,2,3))
  }
  /**
   * Convert to WriteChars
   */
  def asWriteChars {
    import scalax.io.Codec
    import scalax.io.JavaConverters._
    
    new java.io.File("io").asBinaryWriteChars(Codec.UTF8).write("This is a message in UTF8")
    new StringWriter().asWriteChars
  }
  /**
   * Convert to ReadChars
   */
  def asReadChars {
    import scalax.io.{JavaConverters,Codec,LongTraversable,Line}
    import Line.Terminators.Custom
    import JavaConverters._

    val lines:Traversable[String] = new java.io.File("io").asBinaryReadChars(Codec.UTF8).lines()
    val webpage:String = new java.io.StringReader("hello").asReadChars.slurpString
    val wrapTraversable: LongTraversable[String] = "hello".asReadChars.lines(Custom(";"))
  }
}