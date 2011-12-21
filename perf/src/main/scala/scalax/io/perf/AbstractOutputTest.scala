package scalax.io.perf

import Utils.generateTestData
import java.nio._
import sperformance.dsl.PerformanceDSLTest
import sperformance.Keys
import scalax.io.Output
import scalax.io._
import Line.Terminators._
import util.Random._
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream }
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.io.InputStream
import java.nio.channels.Channels
import java.io.OutputStream

abstract class AbstractOutputTest extends PerformanceDSLTest {
  val MaxSize: Int
  val Inc: Int
  val From: Int

  def newOut: () => OutputStream
  def newOutResource = {
    val out = newOut
    Resource.fromOutputStream(out())
  }
  def withSizeDef[U](f: Int => U) = withSize from (From) upTo MaxSize by Inc withSetup (f)

  performance of "Output" in {
    having attribute (Keys.WarmupRuns -> 10) in {
      measure method "write byte array" in {
        withSizeDef { size =>
          (size, generateTestData(size, 1).getBytes("UTF-8"))
        } run {
          case (size, data) =>
            val out = newOutResource
            out.write(data)
        }
      }
      measure method "write byte array" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std java io") in {
            withSizeDef { size =>
              (size, generateTestData(size, 1).getBytes("UTF-8"))
            } run {
              case (size, data) =>
                val out = newOut()
                out.write(data)
                out.close()
            }
          }
        }
      }
      measure method "write byte buffer" in {
        withSizeDef { size =>
          (size, ByteBuffer.wrap(generateTestData(size, 1).getBytes("UTF-8")))
        } run {
          case (size, data) =>
            val out = newOutResource
            out.write(data)
        }
      }
      measure method "write byte buffer" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              (size, ByteBuffer.wrap(generateTestData(size, 1).getBytes("UTF-8")))
            } run {
              case (size, data) =>
                val out = newOut()
                Channels.newChannel(out).write(data)
                out.close()
            }
          }
        }
      }
      measure method "write partial byte array" in {
        withSizeDef { size =>
          (size, generateTestData(size, 1).getBytes("UTF-8"))
        } run {
          case (size, data) =>
            val out = newOutResource
            out.write(data.slice(size / 4, size / 4))
        }
      }
      measure method "write partial byte array" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std java io") in {
            withSizeDef { size =>
              (size, generateTestData(size, 1).getBytes("UTF-8"))
            } run {
              case (size, data) =>
                val out = newOut()
                out.write(data, size / 4, size / 4)
                out.close()
            }
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
      measure method "write string" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "apache ioutils") in {
            withSizeDef { size =>
              (size, generateTestData(size, 1))
            } run {
              case (size, data) =>
                val out = newOut()
                IOUtils.write(data, out, "UTF-8")
                out.close()
            }
          }
        }
      }
      measure method "write strings" in {
        withSizeDef { size =>
          (size, generateTestData(size, 3).split("\n"))
        } run {
          case (size, data) =>
            val out = newOutResource
            out.writeStrings(data)(Codec.UTF8)
        }
      }
      measure method "write strings" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "apache ioutils") in {
            withSizeDef { size =>
              (size, generateTestData(size, 3).split("\n"))
            } run {
              case (size, data) =>
                val out = newOut()
                data.foreach(IOUtils.write(_, out, "UTF-8"))
                out.close()
            }
          }
        }
      }
      measure method "write char array" in {
        withSizeDef { size =>
          (size, generateTestData(size, 3).toArray)
        } run {
          case (size, data) =>
            val out = newOutResource
            out.writeChars(data)(Codec.UTF8)
        }
      }
      measure method "write char array" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "apache ioutils") in {
            withSizeDef { size =>
              (size, generateTestData(size, 3).toArray)
            } run {
              case (size, data) =>
                val out = newOut()
                IOUtils.write(data, out, "UTF-8")
                out.close()
            }
          }
        }
      }
    }
  }
}