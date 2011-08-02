package scalax.io.perf

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
import java.io.Reader


abstract class AbstractReaderCharsTest extends PerformanceDSLTest {
  implicit val codec = Codec.UTF8

  def MaxSize:Int
  def Inc:Int
  def From:Int
  def WarmUpRuns:Int

  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   * 
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep):() => Reader
  def newInResource(size: Int, lines: Int = 2, term: String = NewLine.sep):ReadChars = {
    val in = newIn(size, lines, term)
    fromReader(in())
    
  }
  def generateTestData(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val lineStrings = 1 to lines map { _ =>
      nextString(size).replaceAll("\n", " ")
    }
    lineStrings mkString term
  }
  
  def withSizeDef[U](f: Int => U) = withSize from(From) upTo MaxSize by Inc withSetup (f)
  
  performance of "ReadChars" in {
    having attribute (Keys.WarmupRuns -> WarmUpRuns) in {
/*      measure method "copyData" in {
        withSizeDef { size =>
          val in = newInResource(size)
          val out = fromOutputStream(new ByteArrayOutputStream())
          (in, out)
        } run {
          case (in, out) =>
            in.copyData(out)
        }
      }
      measure method "copyData" in {
        having attribute ("version", "Apache IO copy bytes") in {
          withSizeDef { size =>
            val in = newIn(size)
            val out = new ByteArrayOutputStream()
            (in, out)
          } run {
            case (in, out) =>
              IOUtils.copy(in(), out)
          }
        }
      }*/
      measure method "chars" in {
        withSizeDef { size =>
          newInResource(size)
        } run { in =>
          in.chars.foreach(_ => ())
        }
      }
      measure method "chars" in {
        having attribute ("version", "java.io read chars with InputStreamReader") in {
          withSizeDef { size =>
            (size, newIn(size))
          } run {
            case (size, in) =>
              val reader = in()
              val buffer = new Array[Char](size * 2)
              var read = 0

              do {
                read = reader.read(buffer)
              } while (read > 0)
              reader.close()
          }
        }
      }
      measure method "lines newline" in {
        having attribute ("version", "Apache IOUtils line") in {
          withSizeDef { size =>
            newIn(5, size, NewLine.sep)
          } run { inFunc =>
            val in = inFunc()
            val iter = IOUtils.lineIterator(in)
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines newline" in {
        withSizeDef { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          in.lines(Line.Terminators.NewLine).foreach(_ => ())
        }
      }
      measure method "lines Auto" in {
        withSizeDef { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          in.lines(Line.Terminators.Auto()).foreach(_ => ())
        }
      }
      measure method "lines Auto" in {
        having attribute ("version", "Apache IOUtils line") in {
          withSizeDef { size =>
            newIn(5, size, NewLine.sep)
          } run { inFunc =>
            val in = inFunc()
            val iter = IOUtils.lineIterator(in)
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines CR" in {
        withSizeDef { size =>
          newInResource(5, size, CarriageReturn.sep)
        } run { in =>
          in.lines(CarriageReturn).foreach(_ => ())
        }
      }
      measure method "lines CR" in {
        having attribute ("version", "Apache IOUtils line") in {
          withSizeDef { size =>
            newIn(5, size, CarriageReturn.sep)
          } run { inFunc =>
            val in = inFunc()
            val iter = IOUtils.lineIterator(in)
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines RN" in {
        withSizeDef { size =>
          newInResource(5, size, RNPair.sep)
        } run { in =>
          assert(in.lines(RNPair).forall{_.last != '\n'})
        }
      }
      measure method "lines RN" in {
        having attribute ("version", "toString split on terminator") in {
          withSizeDef { size =>
            newIn(5, size, RNPair.sep)
          } run { inFunc =>
            val in = inFunc()
            IOUtils.toString(in).split("\r\n")
            in.close()
          }
        }
      }
      measure method "lines Custom" in {
        withSizeDef { size =>
          newInResource(5, size, "**")
        } run { in =>
          in.lines(Custom("**")).foreach(_ => ())
        }
      }
      measure method "lines Custom" in {
        having attribute ("version", "java.io Reader") in {
          withSizeDef { size =>
            newIn(5, size, "**")
          } run { inFunc =>
            val in = inFunc()
            IOUtils.toString(in).split("\\*\\*")
            in.close()
          }
        }
      }
    }
  }

}