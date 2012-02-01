package scalaio.test

import scalax.io.{ Resource, Seekable }
import org.junit.rules.TemporaryFolder
import org.junit.{ After, Before }
import java.io.{ RandomAccessFile, File }
import scalax.io._

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
  override def correctly_closes_resources : Unit = if(canAddCloseAction) super.openSeekableReadAndWrite else ()
  override def openSeekableReadAndWrite: Unit = if(canAddCloseAction) super.openSeekableReadAndWrite else ()
  override def openSeekable: Unit = if(canAddCloseAction) super.openSeekable else ()
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
    Resource.fromFile(new File(folder.getRoot, "testfile").getAbsolutePath)
  }
}

class FileSeekableTest extends AbstractFileSeekableTest {
  def openResource(closeAction:CloseAction[SeekableByteChannel]): Seekable = {
    Resource.fromFile(new File(folder.getRoot, "testfile"))
  }
}

class RandomAccessFileSeekableTest extends AbstractFileSeekableTest {
  def openResource(closeAction:CloseAction[SeekableByteChannel]): Seekable = {
    val file = new File(folder.getRoot, "testfile")
    Resource.fromRandomAccessFile(new RandomAccessFile(file, "rw"))
  }
}

class StraightCreationSeekableTest extends AbstractFileSeekableTest {
  override def canAddCloseAction = true
  def openResource(closeAction:CloseAction[SeekableByteChannel]): Seekable = {
    def channel = SeekableByteChannel.fromFile(new File(folder.getRoot, "testfile").getAbsolutePath)
    new SeekableByteChannelResource(channel, closeAction, () => Some(channel(List(StandardOpenOption.Read)).size))
  }
}
