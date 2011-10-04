package scalax.io.perf

/**
 * Run all performance tests
 */
object PerformanceSuite {

  def main(args: Array[String]): Unit = {  
    Main.runTests(
        // -----  SmallSets in memory input
        () => new stream.input.SmallSetsInMemoryInputStreamTest,
        () => new channel.input.SmallSetsInMemoryReadableByteChannelTest,
        () => new reader.SmallSetsInMemoryReadCharsTest,
        
        // -----  SmallSets in memory output
        () => new stream.output.SmallSetsInMemoryOutputStreamTest,
        () => new channel.output.SmallSetsInMemoryWritableByteChannelTest,
        () => new writer.SmallSetsInMemoryWriteCharsTest,
        
        // -----  SmallSets from file input
        () => new stream.input.SmallSetsFromFileInputStreamTest,
        () => new channel.input.SmallSetsFromFileReadableByteChannelTest,
        () => new channel.input.SmallSetsFromFileSeekableByteChannelTest,
        () => new reader.SmallSetsFromFileReadCharsTest,

        // -----  SmallSets to file output
        () => new stream.output.SmallSetsToFileOutputStreamTest,
        () => new channel.output.SmallSetsToFileWritableByteChannelTest,
        () => new channel.output.SmallSetsToFileSeekableByteChannelTest,
        () => new writer.SmallSetsToFileWriteCharsTest,
        
        // -----  SmallMediumSets in memory input
        () => new stream.input.SmallMediumSetsFromMemoryInputStreamTest,
        () => new channel.input.SmallMediumSetsFromMemoryReadableByteChannelTest,
        () => new reader.SmallMediumSetsFromMemoryReadCharsTest,
        
        // -----  SmallMediumSets in memory output
        () => new stream.output.SmallMediumSetsInMemoryOutputStreamTest,
        () => new channel.output.SmallMediumSetsInMemoryWritableByteChannelTest,
        () => new writer.SmallMediumSetsInMemoryWriteCharsTest,
        
        // -----  SmallMediumSets from file input
        () => new stream.input.SmallMediumSetsFromFileInputStreamTest,
        () => new channel.input.SmallMediumSetsFromFileReadableByteChannelTest,
        () => new channel.input.SmallMediumSetsFromFileSeekableByteChannelTest,
        () => new reader.SmallMediumSetsFromFileReadCharsTest,
        
        // -----  SmallMediumSets to file output
        () => new stream.output.SmallMediumSetsToFileOutputStreamTest,
        () => new channel.output.SmallMediumSetsToFileWritableByteChannelTest,
        () => new channel.output.SmallMediumSetsToFileSeekableByteChannelTest,
        () => new writer.SmallMediumSetsToFileWriteCharsTest,
        
        // -----  Seekable tests
        () => new seekable.SmallMediumSetFileSeekable,
        () => new seekable.SmallSetFileSeekable,
        () => new seekable.SmallMediumSetMemorySeekable,
        () => new seekable.SmallSetMemorySeekable,
        
        // -----  Miscellaneous Tests
        () => new file.HardDriveScan,
        () => new iterator.CloseableIteratorPerformanceTest
        )
  }
}