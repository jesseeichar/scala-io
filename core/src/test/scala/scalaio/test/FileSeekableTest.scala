package scalaio.test

import org.junit.rules.TemporaryFolder
import org.junit.{ After, Before }
import scalax.io._
import java.io.{IOException, RandomAccessFile, File}

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
  lazy val file = new File(folder.getRoot, "testfile")
  def forceErrorOnAccess = {
    file.delete()
    file.getParentFile.setReadOnly()
  }

  def canAddCloseAction = false
  override def scalaIoException_On_Write_Error_by_default {
    intercept[IOException] {
      errorOnWriteOut.write("hi")
    }
  }
  override def scalaIoException_On_Read_Error_by_default {
    intercept[IOException] {
      input(AbstractInputTests.ErrorOnRead,() => (), () => ()).bytes.head
    }
  }
}
class FileStringSeekableTest extends AbstractFileSeekableTest {
  def openResource(openFunction: () => Unit, closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    folder.delete()
    folder.create()
    Resource.fromFile(file.getAbsolutePath).addCloseAction(closeAction)
  }
  override def truncatesUnderlyingSinkEachOpen = true

}

class FileSeekableTest extends AbstractFileSeekableTest {
  def openResource(openFunction: () => Unit, closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    folder.delete()
    folder.create()
    Resource.fromFile(file).addCloseAction(closeAction)
  }
  override def truncatesUnderlyingSinkEachOpen = true

}

class RandomAccessFileSeekableTest extends AbstractFileSeekableTest {
  def openResource(openFunction: () => Unit, closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    folder.delete()
    folder.create()
    file.getParentFile.mkdirs()
    file.createNewFile()
    Resource.fromRandomAccessFile(new RandomAccessFile(file, "rw")).addCloseAction(closeAction)
  }

  override def truncatesUnderlyingSinkEachOpen = true
}

class StraightCreationSeekableTest extends AbstractFileSeekableTest {
  override def canExecuteOpenFunction = true
  override def canAddCloseAction = true
  def openResource(openFunction: () => Unit, closeAction: CloseAction[SeekableByteChannel]): SeekableResource[_] = {
    folder.delete()
    folder.create()
    def channel = {
      openFunction()
      SeekableByteChannel.fromFile(file.getAbsolutePath)
    }
    Resource.fromSeekableByteChannel(channel).addCloseAction(closeAction)
  }
  override def truncatesUnderlyingSinkEachOpen = true

}
