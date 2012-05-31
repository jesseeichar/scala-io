package scalax.io.perf
package stream
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

class SmallMediumSetsFromFileInputStreamTest 
  extends AbstractInputTest 
  with FileBase with SmallMediumDataSet
  
object SmallMediumSetsFromFileInputStreamRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsFromFileInputStreamTest)
  }
}

// ----------------------------------------------------------------

class SmallMediumSetsFromMemoryInputStreamTest 
  extends AbstractInputTest 
  with MemoryBase  with SmallMediumDataSet

object SmallMediumSetsFromMemoryInputStreamRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsFromMemoryInputStreamTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsFromFileInputStreamTest  
  extends AbstractInputTest 
  with FileBase with SmallDataSet 

object SmallSetsFromFileInputStreamRunner {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsFromFileInputStreamTest)
  }
}

// ----------------------------------------------------------------

class SmallSetsInMemoryInputStreamTest 
  extends AbstractInputTest 
  with MemoryBase with SmallDataSet
  
object SmallSetsInMemoryInputStreamTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsInMemoryInputStreamTest)
  }
}