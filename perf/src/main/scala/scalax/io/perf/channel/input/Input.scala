package scalax.io.perf
package channel
package input

import Utils._
import scalax.io._
import sperformance.Keys.WarmupRuns
import sperformance.dsl._
import util.Random._
import Resource._
import Line.Terminators._
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream }
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

// ----------------------------------------------------------------

class SmallMediumSetsFromFileReadableByteChannelTest
  extends AbstractReadableByteChannelInputTest
  with FileBase with SmallMediumDataSet

object SmallMediumSetsFromFileReadableByteChannelRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsFromFileReadableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallMediumSetsFromFileSeekableByteChannelTest
  extends AbstractReadableByteChannelInputTest
  with SeekableBase with SmallMediumDataSet {
  override def allowRandomAccess = true
  override def newInResource(size: Int, lines: Int = 2, term: String = NewLine.sep): Input = {
    val in = newIn(size, lines, term)
    Resource.fromSeekableByteChannel(in())
  }
}

object SmallMediumSetsFromFileSeekableByteChannelRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsFromFileSeekableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallMediumSetsFromMemoryReadableByteChannelTest
  extends AbstractReadableByteChannelInputTest
  with MemoryBase with SmallMediumDataSet

object SmallMediumSetsFromMemoryReadableByteChannelRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsFromMemoryReadableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsFromFileReadableByteChannelTest
  extends AbstractReadableByteChannelInputTest
  with FileBase with SmallDataSet

object SmallSetsFromFileReadableByteChannelRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsFromFileReadableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsFromFileSeekableByteChannelTest
  extends AbstractReadableByteChannelInputTest
  with SeekableBase with SmallDataSet {
  override def allowRandomAccess = true
  override def newInResource(size: Int, lines: Int = 2, term: String = NewLine.sep): Input = {
    val in = newIn(size, lines, term)
    Resource.fromSeekableByteChannel(in())
  }
}

object SmallSetsFromFileSeekableByteChannelRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsFromFileSeekableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsInMemoryReadableByteChannelTest
  extends AbstractReadableByteChannelInputTest
  with MemoryBase with SmallDataSet

object SmallSetsInMemoryReadableByteChannelTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsInMemoryReadableByteChannelTest)
  }
}