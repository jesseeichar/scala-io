package scalax.io
package perf
package seekable

class SmallMediumSetFileSeekable extends AbstractFileSeekableTest {
  val MaxSize = 15000
  val Inc = 5000
  val From = 5000
  val WarmUpRuns = 100

}

object SmallMediumSetFileSeekable {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallMediumSetFileSeekable)
  }
}
