package scalax.io.perf
package stream
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

class SmallMediumSetsInMemoryOutputStreamTest 
  extends AbstractOutputTest 
  with MemoryBase with SmallMediumDataSet 
  
object SmallMediumSetsInMemoryOutputStreamRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsInMemoryOutputStreamTest)
  }
}
 
// ----------------------------------------------------------------

class SmallMediumSetsToFileOutputStreamTest 
  extends AbstractOutputTest 
  with FileBase with SmallMediumDataSet
  
object SmallMediumSetsToFileOutputStreamRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsToFileOutputStreamTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsInMemoryOutputStreamTest 
  extends AbstractOutputTest 
  with MemoryBase with SmallDataSet 
  
object SmallSetsInMemoryOutputStreamTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsInMemoryOutputStreamTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsToFileOutputStreamTest  
  extends AbstractOutputTest 
  with FileBase with SmallDataSet 

object SmallSetsToFileOutputStreamTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsToFileOutputStreamTest)
  }
}

