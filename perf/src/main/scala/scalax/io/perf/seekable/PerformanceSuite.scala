package scalax.io.perf
package seekable

/**
 * Run all performance tests
 */
object SeekablePerformanceSuite {

  def main(args: Array[String]): Unit = {
    Main.runTests(
        () => new SmallMediumSetFileSeekable,
        () => new SmallSetFileSeekable,
        () => new SmallMediumSetMemorySeekable,
        () => new SmallSetMemorySeekable
        )
  }
}
