import java.io.OutputStream
import java.util.Date
import scalax.io.OutputConverter.{LongConverter, TraversableLongConverter, TraversableByteConverter}

/**
 * Details on how output is converted to bytes and how the design can be extended and used.
 * <div>
 * Many common types of objects can be written to an Output object or seekable object
 * and is converted to bytes by the implicitly selected OutputConverter object
 * </div>
 */
object OutputAndTypeClasses {
  /**
   * The write method of Output and Seekable objects accepts several types of
   * inputs with no effort required on the part of a developer.  Several
   * of the default types including integers, longs, doubles, etc... are
   * have converter implementations.
   */
  def theCommonCases {
    import scalax.io._
    import Resource._

    val out = fromFile("out")
    // Selected Converter is OutputConverter.IntConverter
    out.write(3)
    // Selected Converter is OutputConverter.LongConverter
    out.write(3L)
    // Selected Converter is OutputConverter.ByteConverter
    out.write(3.toByte)
    // Selected Converter is OutputConverter.DoubleConverter
    out.write(3.0)
    // Selected Converter is OutputConverter.IntConverter
    out.write(List[Byte](3))
    // Selected Converter is OutputConverter.LongConverter
    out.write(3L)
    // Selected Converter is OutputConverter.ByteConverter
    out.write(3.toByte)
    // Selected Converter is OutputConverter.DoubleConverter
    out.write(3.0)
    // Selected Converter is OutputConverter.TraversableIntConverter
    out.write(List(1,2,3,4))
  }

  /**
   * In Java when you write an Integer to an OuputStream that integer is
   * treated as a byte and only the lowest value byte of the int is written.
   * <p>Scala IO differs in that an integer is written as 4 bytes and
   * one must explicitely coerce an Int to a byte. The following examples
   * demostrate how one might do that.</p>
   */
  def intsAsBytes {
    import scalax.io._
    import Resource._

    val out = fromFile("out")
    // One of the easiest ways is to coerce the
    // ints into bytes before passing them to a
    // write method
    out.write(List[Byte](1,2,3,4))
    out.write(1.toByte)

    // writeIntsAsBytes (or patchIntsAsBytes) is
    // another good solution
    out.writeIntsAsBytes(List(1,2,3,4))
    out.insertIntsAsBytes(4,List(1,2,3))
    out.patchIntsAsBytes(3,List(1,2,3),OverwriteAll)
    out.appendIntsAsBytes(List(1,2,3))

    // The final option is to pass in the
    //(OutputConverter.IntAsByteConverter) object to the write method:
    out.write(3)(OutputConverter.IntAsByteConverter)
    out.write(List(1,2,3,4))(OutputConverter.TraversableIntAsByteConverter)
  }

  /**
   * Writing Arrays is a special situation because of how
   * Java and Scala use arrays.  For performance Scala uses the
   * Java Array object and coerces them into Traversable objects when
   * the Scala collections methods are needed.  However the implicit resolution
   * will not choose a Traversable*Converter.  Fortunately Scala IO
   * provides several Converters for converting Arrays to bytes.
   * <p>
   * The point of this example is explain that if one is creating a custom
   * converter he will have to consider creating both a
   * OutputConverter[Traversable[_]] as well as a OutputConverter[Array[_]]</p>
   */
  def writingArrays {
    import scalax.io._
    import Resource._

    val out = fromFile("out")
    out.write(Array(1,2,3,4))(OutputConverter.IntAsByteArrayConverter)
    out.write(Array(1,2,3,4))

  }

  /**
   * Writing Strings and characters require that codec object is passed
   * to the Output object so that means the "normal" typeclass design cannot
   * be used to implicitely write characters and strings to the Output.
   * Because of this write,patch,insert,etc... are overloaded with a typeclass
   * version as well as a version that takes a string.
   * <p>
   * The result is that writing strings is a simple exercise but writing characters
   * or Traversables of characters is less trivial.  The examples below show how
   * to write strings and characters. </p>
   */
  def stringsAndCharacters {
    import scalax.io._
    import Resource._

    val out = fromFile("out")

    // codec can be passed implicitely or explicitly
    out.write("A string")(Codec.UTF8)
    implicit val codec = Codec.UTF8
    out.write("c")

    // out.write('c') will not compile since a converter cannot
    // be resolved by the implicit resolution mechanism because
    // character converters require a codec and only concrete
    // objects are resolved.
    out.write('c')(OutputConverter.charToOutputFunction)
    out.write(Set('a','e','i','o','u'))(OutputConverter.charsToOutputFunction)

    // converters can be passed implicitly
      implicit val traversableCharCoverter = OutputConverter.charsToOutputFunction
      out.write(Set('a','e','i','o','u') )
      out.write('a' to 'z')
  }

  /**
   * Declaring custom converters.
   * <p>Naturally being able to write objects other than those defined
   * by Scala IO can be beneficial and it is a simple process.  All that is
   * needed is a new implementation of a OutputConverter which is imported into
   * scopoe.
   * </p><p>The examples below show two design patterns.</p>
   */
  def customDataTypes {
    import scalax.io._
    import Resource._
    import OutputConverter._
    val out = fromFile("out")
    // Simplest design pattern is to create a new implicit object in scope
    implicit object DateConverter extends OutputConverter[Date] {
      def sizeInBytes = 8
      def toBytes(data: Date) = LongConverter.toBytes(data.getTime)
    }

    out.write(new Date(2012,01,01))

    // write, append, patch and insert all follow the same pattern
    out.append(3)

    // The second (an more reusable design pattern) is to create an object
    // that contains the converters that you want to use and then they can be
    // reused through out the code base.
    object CustomConverters {
      case class User(name:String,id:Int)
      // first you need converter for a collection of your type
      implicit object UserTraversableConverter extends OutputConverter[TraversableOnce[User]] {
        def sizeInBytes = 2

        def toBytes(users: TraversableOnce[User]):TraversableOnce[Byte] = {
          // Create a single instance of a buffer for encoding the id value.
          val idBuffer = new OutputConverter.Buffer[Int](4,(byteBuffer,data) => {byteBuffer.putInt(data)})
          users.toIterator.flatMap{
            user =>
              user.name.getBytes("ASCII").toIterator ++ idBuffer.put(user.id)
          }
        }
      }
      // next you need converters for the basic type and arrays
      implicit object UserConverter extends NonTraversableAdapter(UserTraversableConverter)
      implicit object UserArrayConverter extends ArrayAdapter(UserTraversableConverter)
    }

    // finally you can import the definitions into scope and write away
    import CustomConverters._

    out.write(User("Jesse Eichar",888888))
    out.insert(2,User("Jesse",23421))
  }
}
