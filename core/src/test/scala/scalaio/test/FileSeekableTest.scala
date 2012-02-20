package scalaio.test

import scalax.io.{ Resource, Seekable }
import org.junit.rules.TemporaryFolder
import org.junit.{ After, Before }
import java.io.{ RandomAccessFile, File }
import scalax.io._
import scalax.io.managed.SeekableByteChannelResource
import scalax.io.managed.InputStreamResource
import java.io.InputStream

abstract class AbstractFileSeekableTest extends AbstractSeekableTests[SeekableByteChannel] with SeekableTestUtils[SeekableByteChannel] {
  var folder: TemporaryFolder = _

  @Before
  def before {
    folder = new TemporaryFolder()
    folder.create()
  }

  @After
  def after {
    folder.delete()
  }

  def canAddCloseAction = false
}

class FileStringSeekableTest extends AbstractFileSeekableTest {
  def deleteResource = folder.delete()
  def openResource(closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    folder.delete()
    folder.create()
    Resource.fromFile(new File(folder.getRoot, "testfile").getAbsolutePath).addCloseAction(closeAction)
  }

}

class FileSeekableTest extends AbstractFileSeekableTest {
  def deleteResource = folder.delete()
  def openResource(closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    folder.delete()
    folder.create()
    Resource.fromFile(new File(folder.getRoot, "testfile")).addCloseAction(closeAction)
  }
}

class RandomAccessFileSeekableTest extends AbstractFileSeekableTest {
  def deleteResource = folder.delete()
  def openResource(closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    folder.delete()
    folder.create()
    val file = new File(folder.getRoot, "testfile")
    Resource.fromRandomAccessFile(new RandomAccessFile(file, "rw")).addCloseAction(closeAction)
  }
}

class StraightCreationSeekableTest extends AbstractFileSeekableTest {
  override def canAddCloseAction = true
  def deleteResource = folder.delete()
  def openResource(closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    folder.delete()
    folder.create()
    def channel = SeekableByteChannel.fromFile(new File(folder.getRoot, "testfile").getAbsolutePath)
    Resource.fromSeekableByteChannel(channel).addCloseAction(closeAction)
  }
}
