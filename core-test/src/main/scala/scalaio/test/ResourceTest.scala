package scalaio.test

import org.junit.Assert._
import org.junit.rules.TemporaryFolder
import org.junit.{After, Before, Rule, Test}
import scalax.io.nio.SeekableFileChannel
import scalax.io._
import java.nio.channels.ByteChannel
import java.io._

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

    val resource = Resource.fromFileString(file.getAbsolutePath)
    val bytes = resource.byteArray
    assertArrayEquals(dataAsBytes,bytes)
    assertEquals(data,resource.slurpString)
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


      fileResource match {
        case b:BufferableOutputResource[_,_] =>
          val buffered = b.buffered
          buffered.write("hip")
          buffered.write("hee")
        case _ => ()
      }

    }

    def testInReuse(fileResource:InputResource[_]) {

      val reader = fileResource.reader
      reader.slurpString
      reader.slurpString

      val in = fileResource.inputStream
      in.slurpString
      in.slurpString

      fileResource match {
        case b:BufferableInputResource[_,_] =>
          val buffered = b.buffered
          buffered.slurpString
          buffered.slurpString
        case _ => ()
      }
    }

    testOutReuse(Resource.fromFile(file))
    testOutReuse(Resource.fromByteChannel(new RandomAccessFile(file,"rw").getChannel))
    testOutReuse(Resource.fromOutputStream(new ByteArrayOutputStream()))
    testOutReuse(Resource.fromBufferedOutputStream(new BufferedOutputStream(new ByteArrayOutputStream())))

    testInReuse(Resource.fromFile(file))
    testInReuse(Resource.fromByteChannel(new RandomAccessFile(file,"rw").getChannel))

    testInReuse(Resource.fromInputStream(new ByteArrayInputStream("hi".getBytes)))
    testInReuse(Resource.fromBufferedInputStream(new BufferedInputStream(new ByteArrayInputStream("hi".getBytes))))

    // no exception is a pass
  }

}