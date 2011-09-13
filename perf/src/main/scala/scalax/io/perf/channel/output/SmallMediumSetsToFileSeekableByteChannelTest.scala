package scalax.io.perf
package channel
package output

import Utils._
import scalax.io._
import scalax.io.Line.Terminators._
import Resource._
import util.Random._
import sperformance.dsl._

class SmallMediumSetsToFileSeekableByteChannelTest
  extends AbstractWritableByteChannelOutputTest
  with SeekableBase {

  val MaxSize = 15000
  val Inc = 5000
  val From = 5000
  val WarmUpRuns = 100
  val WarmUpRunsForLines = 100

  override def newOutResource = {
    val out = newOut
    Resource.fromSeekableByteChannel(out())
  }

}

object SmallMediumSetsToFileSeekableByteChannelTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetsToFileSeekableByteChannelTest)
  }
}
