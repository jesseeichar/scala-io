package scalax.io.perf
package reader

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

class SmallMediumSetsFromFileReadCharsTest 
	extends AbstractReadCharsTest 
	with FileBase with SmallMediumDataSet
	
object SmallMediumSetsFromFileReadCharsRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsFromFileReadCharsTest)
  }
}

// ----------------------------------------------------------------

class SmallMediumSetsFromMemoryReadCharsTest 
	extends AbstractReadCharsTest 
	with MemoryBase  with SmallMediumDataSet

object SmallMediumSetsFromMemoryReadCharsRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsFromMemoryReadCharsTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsFromFileReadCharsTest  
	extends AbstractReadCharsTest 
	with FileBase with SmallDataSet 

object SmallSetsFromFileReadCharsRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsFromFileReadCharsTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsInMemoryReadCharsTest 
	extends AbstractReadCharsTest 
	with MemoryBase with SmallDataSet
	
object SmallSetsInMemoryReadCharsTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsInMemoryReadCharsTest)
  }
}