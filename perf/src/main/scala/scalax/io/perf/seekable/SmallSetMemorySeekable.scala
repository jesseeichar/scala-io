package scalax.io
package perf
package seekable

class SmallSetMemorySeekable extends AbstractArrayBufferSeekableTest {
  val MaxSize = 50
  val Inc = 25
  val From = 1
  val WarmUpRuns = 1000

}

object SmallSetMemorySeekable {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetMemorySeekable)
  }
}
