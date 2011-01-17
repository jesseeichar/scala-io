import java.io.{BufferedReader, FileReader, FileOutputStream, StringReader}

object BasicIO {

  /**
   * The right way to convert java IO objects to Scala IO objects
   */
  def javaToScalaRightWay {
    import scalax.io.Resource
    val in = Resource.fromReader(new StringReader("hello"))

    val numVowels = in.chars.filter("aeiou" contains _).size

    // resource can be reused because the *creation* of the reader is passed to fromReader
    // the creation code is captured so Resource can instantiate a new reader for each use of the resource
    val numNumbers = in.chars.filter('0' to '9' contains _)
  }

  /**
   * A discouraged method of creating Scala IO objects from java objects
   */
  def javaToScalaWrongWay {
    import scalax.io.Resource

    val reader = new StringReader("hello")
    // the fromReader method is passed a reference to a reader
    // this means the Resource can only be used a single time
    // only do this if you are passed a resource from a method and have
    // no way of constructing the resource within the fromReader method.
    val in = Resource.fromReader(reader)

    val numVowels = in.chars.filter("aeiou" contains _).size

    // *BOOM!* the second use will result in an exception because
    // Resource does not have access to the construction of the reader
    // just the reference to a previously created reader
    val numNumbers = in.chars.filter('0' to '9' contains _)

    // If you need a code block to construct the resource consider the following pattern:
    val in2 = Resource.fromReader {
      val string = "hello"
      new StringReader(string)
    }
  }

  /**
   * Examples of basic IO 
   */
  def basicInput {
    import scalax.io._
    import scalax.io.Resource
    import java.net.URL
    import java.io.{
      InputStreamReader
    }

    // Note that in these example streams are closed automatically
    // Also note that normally a constructed stream is not passed to factory method because most factory methods are by-name parameters (=> R)
    // this means that the objects here can be reused without worrying about the stream being previously emptied
    val url = new URL("www.scala-lang.org")

    val input:Input = Resource.fromInputStream(url.openStream())

    // The simplest way to read data is to read bytes from an Input object
    val bytes: ResourceView[Byte] = input.bytes

    // you can also get the characters and strings from an Input object but you need a codec for decoding the bytes
    val chars: ResourceView[Char] = input.chars(Codec.UTF8)

    implicit val defaultCodec: Codec = Codec.UTF8

    // by declaring an _implicit_ codec I do not need to declare the codec explicitly in the next examples
    val chars2: ResourceView[Char] = input.chars

    // TODO make Lines return a ResourceView[String]
    // one can also iterate across all lines.  The line ending can be autodetected or can be explicitly declared
    val lines_Autodetect: Traversable[String] = input.lines(Line.Terminators.Auto())
    val lines_NewLineChar: Traversable[String] = input.lines(Line.Terminators.NewLine)
    val lines_CarriageReturn: Traversable[String] = input.lines(Line.Terminators.CarriageReturn)
    val lines_BothCarriageAndNewLine: Traversable[String] = input.lines(Line.Terminators.Pair)
    val lines_CustomLineTerminator: Traversable[String] = input.lines(Line.Terminators.Custom("|"))
    val lines_KeepTerminator = input.lines(includeTerminator = true)

    // In some cases a ReadChars object is more useful.  One advantage is that the codec is already specified so the
    // codec is not needed to read characters.  Also if you start with a Reader object only a ReadChars object can
    // be constructed
    Resource.fromInputStream(url.openStream()).reader(defaultCodec).lines() foreach println _

    // Example of constructing a ReadChars object from a Reader
    Resource.fromReader(new InputStreamReader(url.openStream())).lines() foreach println _
  }

  /**
   * Basic output options
   */
  def basicOutput {
    import scalax.io._
    import scalax.io.Resource
    import java.io.{
      ByteArrayOutputStream,FileOutputStream,
      PrintStream, OutputStreamWriter, BufferedReader
    }

    // Note: The file API is nearly finished allowing one to write directly to files without the
    // cumbersome new FileOutputStream shown below.  This is the "pure" scala IO solution
    Resource.fromOutputStream(new FileOutputStream("scala.html")) write "data".getBytes()
    Resource.fromOutputStream(new FileOutputStream("scala.html")) write Array[Byte](1,2,3)

    // strings and bytes can both be written to Output objects but strings need a Codec
    // for encoding the strings.  As usual the codec can be explicit or implicitly declared
    Resource.fromOutputStream(new ByteArrayOutputStream()).write("howdy")(Codec.UTF8)

    implicit val defaultCodec: Codec = Codec.UTF8

    // An Output object cannot be created from a Writer as a writer may have an unknown codec that is
    // used for encoding the strings and without knowing which coded is being used an Output object
    // cannot be created so a WriteChars object is created
    // WriteChars have the benefit of not needing to have a codec declared since the underlying writer
    // takes care of encoding
    Resource.fromWriter(new OutputStreamWriter(new ByteArrayOutputStream())).writeString("howdy")


    Resource.fromOutputStream(new PrintStream(new ByteArrayOutputStream())).writer.writeString("howdy")

    // Channels can also be wrapped in Resource objects and accessed as normal Input/Output objects
    val resource = Resource.fromWritableByteChannel(new FileOutputStream("file").getChannel)
  }

  /**
   * Demonstration of how to convert Input/Output to buffered counterparts
   */
  def bufferedIO {
    import scalax.io._
    import java.io._

    // All io.Resource objects can be converted to a buffered equivalent if the buffered method is called
    val bufferedInput:Input = Resource.fromInputStream(new FileInputStream("file")).buffered
    val bufferedOutput:Output = Resource.fromOutputStream(new FileOutputStream("file")).buffered
    val bufferedRead:ReadChars = Resource.fromReader(new FileReader("file")).buffered
    val bufferedWrite:WriteChars = Resource.fromWriter(new FileWriter("file")).buffered
  }

  /**
   * When converting bytes to and from characters a Codec is needed for the encoding and decoding.  Unlike Java
   * Scala IO does not have a default it requires that the Codec be declared.  However, to simplify the declaration
   * most methods have implicit codec parameters so the Codec only needs to be declared once.
   *
   * The following examples show reading characters from input streams.  Writing is follows the same patter
   */
  def usingCodecs {
    import scalax.io._
    import java.io._

    val in: InputStreamResource[FileInputStream] = Resource.fromInputStream(new FileInputStream("file"))

    // declare the Codec explicitly
    val string:String = in.slurpString(Codec.UTF8)
    val chars: ResourceView[Char] = in.chars(Codec("UTF8"))

    // create a ReadChars so that Codec only needs to be specified once
    val readChars: ReaderResource[Reader] = in.reader(Codec.ISO8859)
    val string2:String = readChars.slurpString
    val chars2: ResourceView[Char] = readChars.chars

    // Finally you can delcare an implicit val once and all calls will implicitly use that codec
    implicit val codec = Codec.UTF8

    val string3:String = in.slurpString
    val chars3: ResourceView[Char] = in.chars
    val readChars2: ReaderResource[Reader] = in.reader

  }

  /**
   * To simplify certain tasks several objects have a conversion to a AsResource object to make converting the
   * object to a Resource easier
   */
  def javaConversions {
    import scalax.io._
    import JavaConversions._
    import java.io._
    import java.net._

    val seekableResource: SeekableByteChannelResource[SeekableByteChannel] = new File("file").asResource
    val input = new URL("http://www.scala-lang.org")
  }
  /**
   * read comma separated file
   */
  def readCsvFile {
    import scalax.io.Resource

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val resource = Resource.fromBufferedReader(new BufferedReader(new FileReader("csv")))
    val records: Traversable[Array[String]] = resource.lines().map (_ split ',')

    // after this it is normal scala collection type operations
  }

  { // add all bytes in file together
    import scalax.io.{Input,InputStreamResource}
    import java.io.InputStream
    import scalax.io.JavaConversions._
    import java.net.URL

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    // you can use the implicit conversions of JavaConversions to add the asResource method to certain classes like URL
    val someFile:Input = new URL("file://someFile").asResource

    // Actual type is InputStreamResource[InputStream] but that is only needed if you want to convert to a reader
    val someFile2: InputStreamResource[InputStream] = new URL("file://someFile").asResource
    val sum: Int = someFile.bytesAsInts.reduceLeft (_ + _)
  }

  { // quickly (and unsafely) load file into memory

    // first load as strings and remove vowels
    import scalax.io.{InputStreamResource,ReadChars,Codec}
    import scalax.io.JavaConversions._
    import java.net.URL
    import java.io.InputStream

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    // you can use the implicit conversions of JavaConversions to add the asResource method to certain classes like URL
    val someFile: InputStreamResource[InputStream] = new URL("file://someFile").asResource
    // You can convert an InputStreamResource to a _ReadChars_ type if desired.  That means that the codec needs to be
    // defined just once.
    val someReader: ReadChars = someFile.reader(Codec.UTF8)
    val consonants = someFile.slurpString.filterNot (c => "aeiou" contains c)

    // ok now as bytes
    val (small, large) = someFile.byteArray partition (_ < 128)
  }

  { // iterate over all character in file
    import scalax.io.{Input, JavaConversions, Line}
    import JavaConversions._
    import java.net.URL

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    // you can use the implicit conversions of JavaConversions to add the asResource method to certain classes like URL
    val someFile:Input = new URL("file://someFile").asResource
    val doubled: Traversable[String] = for ( c <- someFile.chars ) yield "" + c + c
  }

  { // read and print out all lines in a file
    import scalax.io.{Input, Codec,Line}
    import scalax.io.JavaConversions._
    import java.net.URL

    // see codec example for why codec is required
    implicit val codec = Codec.UTF8

    // you can use the implicit conversions of JavaConversions to add the asResource method to certain classes like URL
    val someFile:Input = new URL("file://someFile").asResource

    // by default the line terminator is stripped and is
    // auto detected
    someFile.lines() foreach println _

    // now do not strip terminator
    someFile.lines (includeTerminator = true) foreach print _

    // now declare explicitly the terminator
    // terminator is restricted to 1 or 2 characters
    someFile.lines (terminator = Line.Terminators.NewLine) foreach println _
  }

  { // explicitly declare the codecs to use
    import scalax.io.{Input,Codec,SeekableByteChannelResource,SeekableByteChannel}
    import scalax.io.JavaConversions._
    import java.net.URL

    // you can use the implicit conversions of JavaConversions to add the asResource method to certain classes like URL
    val someFile:Input = new URL("file://someFile").asResource

    // All methods for reading and writing characters/strings
    // have a codec parameter that used to explicitly declare the
    // codec to use
    someFile.chars(codec = Codec.ISO8859)

    // If there is not a constant for the desired codec
    // one can easily be created
    someFile.lines()(codec = Codec("UTF-16"))
  }

  { // several examples of writing data
    import scalax.io.JavaConversions._
    import java.io.File
    import scalax.io.{Seekable,Codec}
    // see codec example for why codec is required
    implicit val codec = Codec.UTF8

    // you can use the implicit conversions of JavaConversions to add the asResource method to certain classes like URL
    val someFile: Seekable = new File("someFile").asResource

    // write bytes
    // By default the file write will replace
    // an existing file with the new data
    someFile.write (Array (1,2,3) map ( _.toByte))

    // another option for write is openOptions which allows the caller
    // to specify in detail how the write should take place
    // the openOptions parameter takes a collections of OpenOptions objects
    // which are filesystem specific in general but the standard options
    // are defined in the OpenOption object
    // in addition to the definition common collections are also defined
    // WriteAppend for example is a List(Create, Append, Write)
    someFile.write (List (1,2,3) map (_.toByte))

    // write a string to the file
    someFile.write("Hello my dear file")

    // with all options (these are the default options explicitely declared)
    someFile.write("Hello my dear file")(codec = Codec.UTF8)

    // Convert several strings to the file
    // same options fromString as for write
    someFile.writeStrings( "It costs" :: "one" :: "dollar" :: Nil)

    // Now all options
    someFile.writeStrings("It costs" :: "one" :: "dollar" :: Nil,
                    separator="||\n||")(codec = Codec.UTF8)
  }

  def randomAccess =   { // several examples of writing data
    import scalax.io.{Seekable,Codec,Line}
    import scalax.io.JavaConversions._
    import java.io.File

    // see codec example for why codec is required
    implicit val codec = Codec.UTF8

    // you can use the implicit conversions of JavaConversions to add the asResource method to certain classes like URL
    val someFile: Seekable = new File("someFile").asResource
    someFile.append("append this string")
    someFile.appendStrings(List("s one", "s two"),Line.Terminators.Pair.sep)
  }

}
