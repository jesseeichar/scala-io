package scalax.io.perf
package seekable

/**
 * Run all performance tests
 */
object SeekablePerformanceSuite {

  def main(args: Array[String]): Unit = {  
    Main.runTests(
        SmallMediumSetFileSeekable,
        SmallSetFileSeekable,
        SmallMediumSetMemorySeekable,
        SmallSetMemorySeekable
        )
  }
}