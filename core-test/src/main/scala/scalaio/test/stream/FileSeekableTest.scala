package scalaio.test.stream

import scalaio.test.AbstractSeekableTests
import collection.mutable.ArrayBuffer
import scalax.io.{Codec, Resource, ArrayBufferSeekableChannel, Seekable}
import org.junit.rules.TemporaryFolder
import org.junit.{After, Before}
import java.io.{RandomAccessFile, File}

abstract class AbstractFileSeekableTest extends AbstractSeekableTests {
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
  protected def openResource():Seekable

  def open(data:String) = {
    folder.delete()
    folder.create()
    val r = openResource()
    r write data
    r
  }
}

class FileStringSeekableTest extends AbstractFileSeekableTest {
  def openResource(): Seekable = {
    Resource.fromFile(new File(folder.getRoot, "testfile").getAbsolutePath)
  }
}

class FileSeekableTest extends AbstractFileSeekableTest {
  def openResource(): Seekable = {
    Resource.fromFile(new File(folder.getRoot, "testfile"))
  }
}

class RandomAccessFileSeekableTest extends AbstractFileSeekableTest {
  def openResource(): Seekable = {
    val file = new File(folder.getRoot, "testfile")
    Resource.fromRandomAccessFile(new RandomAccessFile(file,"rw"))
  }
}
