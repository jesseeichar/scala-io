package scalax.io.perf
package channel
package output

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
import java.nio.channels.Channels

class SmallMediumSetsInMemoryWritableByteChannelTest 
  extends AbstractWritableByteChannelOutputTest 
  with MemoryBase with SmallMediumDataSet 
  
object SmallMediumSetsInMemoryWritableByteChannelRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsInMemoryWritableByteChannelTest)
  }
}
 
// ----------------------------------------------------------------

class SmallMediumSetsToFileSeekableByteChannelTest
  extends AbstractWritableByteChannelOutputTest
  with SeekableBase with SmallMediumDataSet {
  override def newOutResource = {
    val out = newOut
    Resource.fromSeekableByteChannel(out())
  }
}

object SmallMediumSetsToFileSeekableByteChannelRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsToFileSeekableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallMediumSetsToFileWritableByteChannelTest 
  extends AbstractWritableByteChannelOutputTest 
  with FileBase with SmallMediumDataSet
  
object SmallMediumSetsToFileWritableByteChannelRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsToFileWritableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsInMemoryWritableByteChannelTest 
  extends AbstractWritableByteChannelOutputTest 
  with MemoryBase with SmallDataSet 
  
object SmallSetsInMemoryWritableByteChannelTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsInMemoryWritableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsToFileSeekableByteChannelTest 
  extends AbstractWritableByteChannelOutputTest 
  with SeekableBase with SmallDataSet {
  override def newOutResource = {
    val out = newOut
    Resource.fromSeekableByteChannel(out())
  }
}


object SmallSetsToFileSeekableByteChannelTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsToFileSeekableByteChannelTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsToFileWritableByteChannelTest  
  extends AbstractWritableByteChannelOutputTest 
  with FileBase with SmallDataSet 

object SmallSetsToFileWritableByteChannelTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsToFileWritableByteChannelTest)
  }
}

