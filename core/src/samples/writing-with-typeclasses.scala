import java.io.OutputStream
import java.util.Date
import scalax.io.OutputConverter.{LongConverter, TraversableLongConverter, TraversableByteConverter}

object OutputAndTypeClasses {
  /**
   * Many common types of objects can be written to an Output object or seekable object and is converted to bytes
   * by the implicitly selected OutputConverter object
   */
  def implicitTypeClassUse {
    import scalax.io._
    import Resource._

    val out = fromFile("out")
    out.write(3)  // Selected Converter is OutputConverter.IntConverter
    out.write(3L) // Selected Converter is OutputConverter.LongConverter
    out.write(3.toByte) // Selected Converter is OutputConverter.ByteConverter
    out.write(3.0) // Selected Converter is OutputConverter.DoubleConverter
    out.write(List(1,2,3,4)) // Selected Converter is OutputConverter.TraversableIntConverter

    // *IMPORTANT* by default integers are written as 4 bytes no downgraded to a byte
    out.writeIntsAsBytes(List(1,2,3,4))  // This method is used for the writing that you see in Java OutputStreams
    // other option is to pass in the (OutputConverter.IntAsByteConverter) objects to the write method:
    out.write(3)(OutputConverter.IntAsByteConverter)
    out.write(List(1,2,3,4))(OutputConverter.TraversableIntAsByteConverter)

    // This next issue is a slightly odd one.  Since Arrays are pure java objects they are not
    // in fact Traversable objects so (OutputConverter.IntAsByteArrayConverter) needs to be passed
    // as the parameter instead of TraversableIntAsByteConverter.
    out.write(Array(1,2,3,4))(OutputConverter.IntAsByteArrayConverter)

    // Certain types cannot be written like Char because it needs a codec parameter to write
    // the characters for writing characters normal operating overloading is used
    out.write("A string")(Codec.UTF8)  // the codec can be passed in implicitly as demonstrated below

    // out.write('c') will not compile since a converter cannot be looked up by default Use one of the following solutions
    implicit val codec = Codec.UTF8 // naturally both solutions require a codec either implicitly declared or implicitly
    out.write("c")
    out.write('c')(OutputConverter.charToOutputFunction)

    // Only the basic datatypes have OutputConverter implementations but nothing prevents one from implementing their own
    implicit object DateConverter extends OutputConverter[Date] {
      def apply(out: OutputStream, date: Date) = TraversableByteConverter(out,toBytes(date))
      def sizeInBytes = 8
      def toBytes(data: Date) = LongConverter.toBytes(data.getTime)
    }

    out.write(new Date(2012,01,01))

    // write, append, patch and insert all follow the same pattern
    out.append(3)
  }
}
