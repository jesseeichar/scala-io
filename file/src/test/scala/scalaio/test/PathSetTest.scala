package scalaio.test

import org.junit.Test
import org.junit.Assert._
import scalax.file._
import scalax.io.CloseableIterator

class PathSetTest {

  class CloseTracker(var opens: Int = 0, var closes: Int = 0) {
    def assertOpenCloses(expectedOpens: Int = -1) = {
      if (expectedOpens == -1) {
        assertTrue(opens > 0)
      } else {
        assertEquals(expectedOpens, opens)
      }
      assertEquals(opens, closes)
    }
  }
  val roots = Seq(Path("a"), Path("aa"))
  def testSet(
    srcFiles: Traversable[Path] = roots,
    pathFilter: PathMatcher[Path] = PathMatcher.All,
    depth: Int = Int.MaxValue,
    includeRoots: Boolean = false) = {
    var closeTracker = new CloseTracker()
    val set = new BasicPathSet[Path](srcFiles, pathFilter, depth, includeRoots,
      (m: PathMatcher[Path], p: Path) => new CountingIterator(closeTracker, p))

    (closeTracker, set)
  }

  class CountingIterator(closeTracker: CloseTracker, p: Path) extends CloseableIterator[Path] {
    closeTracker.opens += 1
    var done = false
    override def hasNext = !done
    override def next = {
      done = true
      p \ (p.name + p.name(0))
    }
    override def doClose = {
      closeTracker.closes += 1
      Nil
    }
  }

  private def checkCloses(f: PathSet[Path] => Any): Unit = {
    checkCloses(-1, f)
  }
  private def checkCloses(expectedOpens: Int, f: PathSet[Path] => Any): Unit = {
    val (tracker, set) = testSet()
    f(set)
    tracker.assertOpenCloses(expectedOpens)
  }
  @Test
  def is_closed_after_each_usage {
    checkCloses(_.head)
    checkCloses(_.headOption)
    checkCloses(_.foreach(_ => ()))
    checkCloses(0, { _.filter(_.name.size < 10) })
  }
}