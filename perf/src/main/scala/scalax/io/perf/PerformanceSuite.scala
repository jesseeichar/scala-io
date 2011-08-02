package scalax.io.perf

import inputstream._
import channel._
import reader._

/**
 * Run all performance tests
 */
object PerformanceSuite {

  def main(args: Array[String]): Unit = {  
    Main.runTests(
        SmallSetsInMemoryInputStreamTest,
        SmallSetsInMemoryReadableByteChannelTest,
        SmallSetsInMemoryReaderCharsTest,
        SmallSetsFromFileInputStreamTest,
        SmallSetsFromFileReadableByteChannelTest,
        SmallSetsFromFileReaderCharsTest,
        SmallMediumSetsFromMemoryInputStreamTest,
        SmallMediumSetsFromMemoryReadableByteChannelTest,
        SmallMediumSetsFromMemoryReaderCharsTest,
        SmallMediumSetsFromFileInputStreamTest,
        SmallMediumSetsFromFileReadableByteChannelTest,
        SmallMediumSetsFromFileReaderCharsTest
        )
  }
}