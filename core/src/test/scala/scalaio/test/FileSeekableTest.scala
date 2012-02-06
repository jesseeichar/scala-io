package scalaio.test

import scalax.io.{ Resource, Seekable }
import org.junit.rules.TemporaryFolder
import org.junit.{ After, Before }
import java.io.{ RandomAccessFile, File }
import scalax.io._
import scalax.io.managed.SeekableByteChannelResource

abstract class AbstractFileSeekableTest extends AbstractSeekableTests[SeekableByteChannel] {
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
  protected def openResource(closeAction:CloseAction[SeekableByteChannel]): Seekable
  def canAddCloseAction = false
  def open(data: String, closeAction:CloseAction[SeekableByteChannel] = CloseAction.Noop) = {
    folder.delete()
    folder.create()
    val r = openResource(closeAction)
    r write data
    r
  }
}

class FileStringSeekableTest extends AbstractFileSeekableTest {
  def openResource(closeAction:CloseAction[SeekableByteChannel]): Seekable = {
    Resource.fromFile(new File(folder.getRoot, "testfile").getAbsolutePath).updateContext(_.copy(closeAction=closeAction))
  }
}

class FileSeekableTest extends AbstractFileSeekableTest {
  def openResource(closeAction:CloseAction[SeekableByteChannel]): Seekable = {
    Resource.fromFile(new File(folder.getRoot, "testfile")).updateContext(_.copy(closeAction=closeAction))
  }
}

class RandomAccessFileSeekableTest extends AbstractFileSeekableTest {
  def openResource(closeAction:CloseAction[SeekableByteChannel]): Seekable = {
    val file = new File(folder.getRoot, "testfile")
    Resource.fromRandomAccessFile(new RandomAccessFile(file, "rw")).updateContext(_.copy(closeAction=closeAction))
  }
}

class StraightCreationSeekableTest extends AbstractFileSeekableTest {
  override def canAddCloseAction = true
  def openResource(closeAction:CloseAction[SeekableByteChannel]): Seekable = {
    def channel = SeekableByteChannel.fromFile(new File(folder.getRoot, "testfile").getAbsolutePath)
    Resource.fromSeekableByteChannel(channel).updateContext(_.copy(closeAction=closeAction))
  }
}
