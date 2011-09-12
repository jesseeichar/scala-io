package scalax.io
package perf
package seekable

object SmallSetMemorySeekable extends AbstractArrayBufferSeekableTest {
  val MaxSize = 50
  val Inc = 25
  val From = 1
  val WarmUpRuns = 100

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}