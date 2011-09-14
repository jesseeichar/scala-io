package scalax.io.perf
package writer

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

class SmallMediumSetsInMemoryWriteCharsTest 
	extends AbstractWriteCharsTest 
	with MemoryBase with SmallMediumDataSet 
	
object SmallMediumSetsInMemoryWriteCharsRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsInMemoryWriteCharsTest)
  }
}
 
// ----------------------------------------------------------------

class SmallMediumSetsToFileWriteCharsTest 
	extends AbstractWriteCharsTest 
	with FileBase with SmallMediumDataSet
	
object SmallMediumSetsToFileWriteCharsRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsToFileWriteCharsTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsInMemoryWriteCharsTest 
	extends AbstractWriteCharsTest 
	with MemoryBase with SmallDataSet 
	
object SmallSetsInMemoryWriteCharsTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsInMemoryWriteCharsTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsToFileWriteCharsTest  
	extends AbstractWriteCharsTest 
	with FileBase with SmallDataSet 

object SmallSetsToFileWriteCharsTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsToFileWriteCharsTest)
  }
}

