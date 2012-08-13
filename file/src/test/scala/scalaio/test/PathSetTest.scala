package scalaio.test

import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.Assert._
import scalax.file._
import scalax.io.CloseableIterator
import util.control.Breaks._

class PathSetTest {

  var dir = None: Option[Path]
  @After
  def after() {
    dir.foreach(_.deleteRecursively(true, true))
  }
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

  def testSet(
    pathFilter: PathMatcher[Path] = PathMatcher.All,
    depth: Int = Int.MaxValue,
    includeRoots: Boolean = false) = {
    after()
    dir = Some(Path.createTempDirectory())
    val srcFiles = Seq(dir.get / "a", dir.get / "aa")
    srcFiles.foreach(_.createDirectory())
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
      val p2 = p \ (p.name + p.name(0))
      p2.createDirectory()
      p2
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
  @Test def head_is_closed_after_each_usage = checkCloses(1, _.head)
  @Test def headOption_is_closed_after_each_usage = checkCloses(1, _.headOption)
  @Test def foreach_is_closed_after_each_usage = {
    var visited = 0
    checkCloses(10, x =>
      breakable {
        x.foreach { p =>
          visited += 1
          if (visited == 10) break
          ()
        }
      })

    assertEquals(10, visited)
  }
  @Test def filter_does_not_open_resource = checkCloses(0, { _.filter(_.name.size < 10) })
  @Test def map_does_not_open_resource = checkCloses(0, { _.map(_.name.size) })
  @Test def flatMap_does_not_open_resource = checkCloses(0, { _.flatMap(_.name) })
  @Test def collect_does_not_open_resource = checkCloses(0, { _.collect { case p => p.name } })
  @Test def take_does_not_open_resource = checkCloses(0, { _.take(5) })
  @Test def drop_does_not_open_resource = checkCloses(0, { _.drop(5) })
  @Test def slice_does_not_open_resource = checkCloses(0, { _.slice(5, 10) })
  @Test def slice_foreach_is_closed_after_each_usage = {
    var visited = 0
    checkCloses(10, { _.slice(5, 10).foreach { p => visited += 1 } })
    assertEquals(5, visited)
  }
}