package scalax.io.perf

import Utils._
import scalax.io._
import sperformance.Keys
import sperformance.dsl._
import util.Random._
import Resource._
import Line.Terminators._
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream }
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.channels.WritableByteChannel
import java.io.Writer
import java.io.BufferedWriter

abstract class AbstractWriteCharsTest extends PerformanceDSLTest {

  def MaxSize: Int
  def Inc: Int
  def From: Int

  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   *
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  def newOut: () => Writer

  def newOutResource: WriteChars = {
    val out = newOut
    fromWriter(out())
  }
  def withSizeDef[U](f: Int => U) = withSize from (From) upTo MaxSize by Inc withSetup (f)

  performance of "WriteChars" in {
    having attribute (Keys.WarmupRuns -> 10) in {

      measure method "write string" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java writer") in {
            withSizeDef { size =>
              (size, generateTestData(size, 1))
            } run {
              case (size, data) =>
                val out = newOut()
                out.write(data)
                out.close()
            }
          }
        }
      }
      measure method "write string" in {
        having attribute ("version", "java bufferedwriter") in {
          withSizeDef { size =>
            (size, generateTestData(size, 1))
          } run {
            case (size, data) =>
              val out = new BufferedWriter(newOut())
              out.write(data)
              out.close()
          }
        }
      }
      measure method "write string" in {
        withSizeDef { size =>
          (size, generateTestData(size, 1))
        } run {
          case (size, data) =>
            val out = newOutResource
            out.write(data)
        }
      }
      measure method "write strings" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java writer") in {
            withSizeDef { size =>
              (size, generateTestData(size, 3).split("\n"))
            } run {
              case (size, data) =>
                val out = newOut()
                data.foreach(out.write(_))
                out.close()
            }
          }
        }
      }
      measure method "write strings" in {
        having attribute ("version", "java buffered writer") in {
          withSizeDef { size =>
            (size, generateTestData(size, 3).split("\n"))
          } run {
            case (size, data) =>
              val out = new BufferedWriter(newOut())
              data.foreach(out.write(_))
              out.close()
          }
        }
      }
      measure method "write strings" in {
        withSizeDef { size =>
          (size, generateTestData(size, 3).split("\n"))
        } run {
          case (size, data) =>
            val out = newOutResource
            out.writeStrings(data)
        }
      }
      measure method "write char array" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java writer") in {
            withSizeDef { size =>
              (size, generateTestData(size, 3).toArray)
            } run {
              case (size, data) =>
                val out = newOut()
                out.write(data)
                out.close()
            }
          }
        }
        measure method "write char array" in {
          having attribute ("version", "java writer") in {
            withSizeDef { size =>
              (size, generateTestData(size, 3).toArray)
            } run {
              case (size, data) =>
                val out = new BufferedWriter(newOut())
                out.write(data)
                out.close()
            }
          }
        }
        measure method "write char array" in {
          withSizeDef { size =>
            (size, generateTestData(size, 3).toArray)
          } run {
            case (size, data) =>
              val out = newOutResource
              out.write(data)
          }
        }
      }
    }
  }
}
