package scalax.io.perf
package iterator

import scalax.io._
import sperformance.Keys.WarmupRuns
import sperformance.dsl._
import util.Random._
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream }
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.io.File
import java.io.FileInputStream
import sperformance.Keys

object Support {
    @inline
  final def testMethod(b: Byte) = {
    b + 1
  }
}
import Support.testMethod
class CloseableIteratorPerformanceTest extends PerformanceDSLTest {
  val From = 1000
  val MaxSize = 100000
  val Inc = (MaxSize - From) / 2
  def withSizeDef[U](f: Int => U) = withSize from (From) upTo MaxSize by Inc withSetup (f)

  var i = 0

  performance of "CloseableIterator" in {
    having attribute (Keys.WarmupRuns -> 100) in {
      /* measure method "next-hasNext" in {
        withSizeDef { i =>

          () => new ByteIter(i)
        } run { iterFac =>
            val iter = iterFac()
          while (iter.hasNext) {
            i += testMethod(iter.next)
          }
        }
      }
      measure method "next-hasNext" in {
        having attribute ("version", "Raw int based iterator") in {
          withSizeDef { i =>
            () => new RawIter(i)
          } run { iterFac =>
            val iter = iterFac()
            while (iter.hasNext) {
              i += testMethod(iter.next)
            }
          }
        }
      }*/
      /*measure method "next-hasNext" in {
        having attribute ("version", "CloseableIter1") in {
          withSizeDef { i =>
            () => new Closeable1(i)
          } run { iterFac =>
            val iter = iterFac()
            while (iter.hasNext) {
              i += testMethod(iter.next)
            }
          }
        }
      }*/
      measure method "foreach" in {
        withSizeDef { i =>
          () => new ByteIter(i)
        } run { iterFac =>
          val iter = iterFac()

          val byteFunc: Function1[Byte, Unit] = new Function1[Byte, Unit] {
            private[this] var i = 0
            def apply(b: Byte) =
              b + 1
          }

          iter.foreach(byteFunc)
        }
      }
      /*measure method "foreach" in {
        having attribute ("version", "CloseableIter1") in {
          withSizeDef { i =>
            () => new Closeable1(i)
          } run { iterFac =>
            val iter = iterFac()
            val f = (b: Byte) => {
              i += testMethod(b)
            }
            iter.foreach(f)
          }
        }
      }*/
      measure method "foreach" in {
        having attribute ("version", "Raw int based iterator") in {
          withSizeDef { i =>
            () => new RawIter(i)
          } run { iterFac =>
            var i = 0
            val iter = iterFac()
            while (iter.hasNext) {
              iter.next + 1
            }
          }
        }
      }
    }
  }

}

object CloseableIteratorPerformanceTest {
  def main(args: Array[String]) {
    Main.runTests(() => new CloseableIteratorPerformanceTest)
  }
  val byte: Byte = 1.toByte
}

import CloseableIteratorPerformanceTest._

class ByteIter(max: Int) extends CloseableIterator[Byte] {
  var c = 0
  override def foreach[@specialized(Unit) U](f: Byte => U) =
    while (hasNext) f(next)

    def next() = {
    c += 1
    byte
  }
  def hasNext = c < max
  protected def doClose(): List[Throwable] = Nil

}
class Closeable1(max: Int) extends Iterator[Byte] {
  var c = 0
  def next() = {
    c += 1
    byte
  }
  def hasNext = c < max
}
class RawIter(max: Int) {
  var c = 0
  def hasNext = c < max
  def next: Byte = {
    c += 1
    byte
  }
}
