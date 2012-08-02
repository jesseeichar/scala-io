package scalaio.test

import org.junit.Assert._
import org.junit.rules.TemporaryFolder
import org.junit.{After, Before, Rule, Test}
import scalax.io.nio.SeekableFileChannel
import scalax.io._
import java.nio.channels.ByteChannel
import java.io._
import scala.concurrent._
import java.net._
import scala.collection.mutable.ArrayBuffer

class ResourceTest {

  var folder:TemporaryFolder = _

  @Before
  def before {
    folder = new TemporaryFolder()
    folder.create()
  }

  @After
  def after {
    folder.delete()
  }



  @Test
  def fromFileString {
    implicit val codec = Codec.UTF8
    val data =
      """some data is in this file
         line 2
         line 3
         line 4
      """
    val dataAsBytes = data.getBytes(codec.charSet)

    val file = new File(folder.getRoot,"data");
    val out = new FileOutputStream(file)
    try {
      out.write(dataAsBytes)
    }

    val resource = Resource.fromFile(file.getAbsolutePath)
    val bytes = resource.byteArray
    assertArrayEquals(dataAsBytes,bytes)
    assertEquals(data,resource.string)
  }


  @Test //@Ignore
  def toString_should_not_open_resource(): Unit = {
    implicit val codec = Codec.UTF8

    val file = new File(folder.getRoot,"data");

    assertFalse(file.exists)

    val fileResource = Resource.fromFile(file)
    fileResource.toString
    assertFalse(file.exists)

    val writer = fileResource.writer
    writer.toString
    assertFalse(file.exists)
  }

  @Test //@Ignore
  def issue_9_file_writer_cannot_be_used_twice(): Unit = {
    implicit val codec = Codec.UTF8

    val file = new File(folder.getRoot,"data");

    assertFalse(file.exists)

    def testOutReuse(fileResource:OutputResource[_]) {
      val writer = fileResource.writer

      writer.write("hi")
      writer.write("ho")

      val out = fileResource.outputStream
      out.write("abc")
      out.write("cba")
    }

    def testInReuse(fileResource:InputResource[_]) {

      val reader = fileResource.reader
      reader.string
      reader.string

      val in = fileResource.inputStream
      in.string
      in.string
    }

    testOutReuse(Resource.fromFile(file))
    testOutReuse(Resource.fromByteChannel(new RandomAccessFile(file,"rw").getChannel))
    testOutReuse(Resource.fromOutputStream(new ByteArrayOutputStream()))

    testInReuse(Resource.fromFile(file))
    testInReuse(Resource.fromByteChannel(new RandomAccessFile(file,"rw").getChannel))

    testInReuse(Resource.fromInputStream(new ByteArrayInputStream("hi".getBytes)))

    // no exception is a pass
  }

  @Test //@Ignore
  def issue_13_size_is_none_even_when_it_could_be_calculated_file(): Unit = {
    val file = new File(folder.getRoot,"file")

    val fResource = Resource.fromFile(file)
    fResource.write(new Array[Byte](0))
    assertEquals(Some(file.length),fResource.size)

    val data = "data"
    fResource.write(data)
    val dataSize = Some(data.getBytes("UTF8").length.toLong)
    assertEquals(Some(file.length),fResource.size);

    assertEquals(Some(file.length),fResource.inputStream.size)

    import JavaConverters._
    assertEquals(Some(file.length),file.asInput.size)

  }

  @Test //@Ignore
  def issue_15_list_size_of_ints_wrong(): Unit = {
    import JavaConverters._

    assertEquals(Some((1 to 5).size),(1 to 5).asInput.size)
    assertEquals(5,(1 to 5).asInput.byteArray.size)

    assertEquals(Some(5),(1 to 5 map {_.toByte}).asInput.size)
    assertEquals(5,(1 to 5 map {_.toByte}).asInput.byteArray.size)
  }

  @Test //@Ignore
  def issue_13_size_is_none_even_when_it_could_be_calculated_url(): Unit = {
    class TestUrlConnectionFactory(size:Int) extends URLStreamHandler(){
      def openConnection(p1: URL): URLConnection = new HttpURLConnection(p1){

        override def getContentLength: Int = size

        override def getInputStream: InputStream = new ByteArrayInputStream(1 to size map {_.toByte} toArray)

        def connect() {}
        def usingProxy(): Boolean = false
        def disconnect() {}
      }
    }
    val unknownSize = new URL("http","localhost",80,"/",new TestUrlConnectionFactory(-1))
    assertEquals(None,Resource.fromURL(unknownSize).size)

    val zeroSize = new URL("http","localhost",80,"/",new TestUrlConnectionFactory(0))
    assertEquals(Some(0),Resource.fromURL(zeroSize).size)

    val positiveSize = new URL("http","localhost",80,"/",new TestUrlConnectionFactory(1000))
    assertEquals(Some(1000),Resource.fromURL(positiveSize).size)

    import JavaConverters._
    assertEquals(Some(1000),positiveSize.asInput.size)
  }

  @Test
  def seekable_size_leaves_open_resource {
    var open = Set[ArrayBufferSeekableChannel]()
    def opener = {
      var chan = new ArrayBufferSeekableChannel(ArrayBuffer[Byte]())(_ => (), c => open -= c)
      open += chan
      chan
    }

    Resource.fromSeekableByteChannel(opener).size
    assertTrue(open.isEmpty)

    Resource.fromByteChannel(opener).size
    assertTrue(open.isEmpty)

    Resource.fromReadableByteChannel(opener).size
    assertTrue(open.isEmpty)


  }

}
