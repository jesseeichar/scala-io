package scalax.io
package perf
package seekable


class SmallSetFileSeekable extends AbstractFileSeekableTest {
  val MaxSize = 50
  val Inc = 25
  val From = 1
  val WarmUpRuns = 100

}

object SmallSetFileSeekable {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetFileSeekable)
  }
}
