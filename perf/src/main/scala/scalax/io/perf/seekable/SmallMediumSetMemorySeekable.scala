package scalax.io
package perf
package seekable

object SmallMediumSetMemorySeekable extends AbstractSeekableTest {
  val MaxSize = 15000
  val Inc = 5000
  val From = 5000
  val WarmUpRuns = 1000

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}