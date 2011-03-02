package scalaio.test

import org.junit.Assert._
import org.junit.rules.TemporaryFolder
import scalax.io.{Codec, Resource}
import java.io.{FileOutputStream, FileWriter, File}
import org.junit.{After, Before, Rule, Test}

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
}