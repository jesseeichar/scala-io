package scalax.io
package perf
package seekable

class SmallMediumSetMemorySeekable extends AbstractArrayBufferSeekableTest {
  val MaxSize = 15000
  val Inc = 5000
  val From = 5000
  val WarmUpRuns = 100

}

object SmallMediumSetMemorySeekable {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetMemorySeekable)
  }
}
