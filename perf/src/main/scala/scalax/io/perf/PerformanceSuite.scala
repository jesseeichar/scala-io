package scalax.io.perf

import inputstream._
import channel._
import reader._
import seekable._
import scalax.io.perf.file.HardDriveScan

/**
 * Run all performance tests
 */
object PerformanceSuite {

  def main(args: Array[String]): Unit = {  
    Main.runTests(
        () => new SmallSetsInMemoryInputStreamTest,
        () => new SmallSetsInMemoryReadableByteChannelTest,
        () => new SmallSetsInMemoryReadCharsTest,
        () => new SmallSetsFromFileInputStreamTest,
        () => new SmallSetsFromFileReadableByteChannelTest,
        () => new SmallSetsFromFileReadCharsTest,
        () => new SmallMediumSetsFromMemoryInputStreamTest,
        () => new SmallMediumSetsFromMemoryReadableByteChannelTest,
        () => new SmallMediumSetsFromMemoryReadCharsTest,
        () => new SmallMediumSetsFromFileInputStreamTest,
        () => new SmallMediumSetsFromFileReadableByteChannelTest,
        () => new SmallMediumSetsFromFileReadCharsTest,
        () => new SmallMediumSetFileSeekable,
        () => new SmallSetFileSeekable,
        () => new SmallMediumSetMemorySeekable,
        () => new SmallSetMemorySeekable,
        () => new HardDriveScan
        )
  }
}