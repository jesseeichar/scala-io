package scalax.io

import sperformance.Keys.WarmupRuns
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

object SmallSetsInMemoryInputTest extends PerformanceDSLTest {
  implicit val codec = Codec.UTF8

  val MaxSize = 50
  val Inc = 25


  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val lineStrings = 1 to lines map { _ =>
      nextString(size).replaceAll("\n"," ")
    }
    val data = lineStrings mkString term
    new ByteArrayInputStream(data.getBytes)
  }
  def newInResource(size: Int, lines: Int = 2, term: String = NewLine.sep) = fromInputStream(newIn(size, lines, term))

  performance of "Input" in {
    having attribute (WarmupRuns -> 1000) in {
      measure method "bytes" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(size)
        } run { in =>
          in.bytes.force
        }
      }
      measure method "bytes" in {
        having attribute ("version", "java.io while loop no buffer") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            newIn(size)
          } run {
            case in =>
              var next = in.read()
              while (next > -1) {
                next = in.read()
              }
              in.close()
          }
        }
      }
      measure method "bytes" in {
        having attribute ("version", "java.io while loop buffered input stream") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            newIn(size)
          } run {
            case in =>
              val bin = new BufferedInputStream(in)
              var next = bin.read()
              while (next > -1) {
                next = bin.read()
              }
              bin.close()
          }
        }
      }
      measure method "bytes" in {
        having attribute ("version", "java.io while loop with buffer") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            (size, newIn(size))
          } run {
            case (size, in) =>
              val buffer = new Array[Byte](size * 2)
              var read = in.read(buffer)
              while (read > 0) {
                read = in.read(buffer)
              }
              in.close
          }
        }
      }
      measure method "bytesAsInts" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(size)
        } run { in =>
          in.bytesAsInts.force
        }
      }
      measure method "bytesAsInts" in {
        having attribute ("version", "java.io while loop impl") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            (newInResource(size).bytesAsInts.size, newIn(size))
          } run {
            case (size, in) =>
              val container = new java.util.ArrayList[Int](size)
              var next = in.read()
              while (next > -1) {
                container.add(next)
                next = in.read()
              }
              in.close
          }
        }
      }
      measure method "byteArray" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(size)
        } run { in =>
          in.byteArray
        }
      }
      measure method "byteArray" in {
        having attribute ("version", "Apache IO read bytes") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            newIn(size)
          } run { in =>
            IOUtils.toByteArray(in);
          }
        }
      }
      measure method "copyData" in {
        withSize upTo MaxSize by Inc withSetup { size =>
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
          withSize upTo MaxSize by Inc withSetup { size =>
            val in = newIn(size)
            val out = new ByteArrayOutputStream()
            (in, out)
          } run {
            case (in, out) =>
              IOUtils.copy(in, out)
          }
        }
      }
      measure method "chars" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(size)
        } run { in =>
          in.chars.force
        }
      }
      measure method "chars" in {
        having attribute ("version", "java.io read chars with InputStreamReader") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            (size, newIn(size))
          } run {
            case (size, in) =>
              val buffer = new Array[Char](size * 2)
              val reader = new InputStreamReader(in, Charset.forName("UTF-8"))
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
          withSize upTo MaxSize by Inc withSetup { size =>
            newIn(5, size, NewLine.sep)
          } run { in =>
            val iter = IOUtils.lineIterator(in, "UTF-8")
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines newline" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          in.lines(Line.Terminators.NewLine).force
        }
      }
      measure method "lines Auto" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          in.lines(Line.Terminators.Auto()).force
        }
      }
      measure method "lines Auto" in {
        having attribute ("version", "Apache IOUtils line") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            newIn(5, size, NewLine.sep)
          } run { in =>
            val iter = IOUtils.lineIterator(in, "UTF-8")
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines CR" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(5, size, CarriageReturn.sep)
        } run { in =>
          in.lines(CarriageReturn).force
        }
      }
      measure method "lines CR" in {
        having attribute ("version", "Apache IOUtils line") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            newIn(5, size, CarriageReturn.sep)
          } run { in =>
            val iter = IOUtils.lineIterator(in, "UTF-8")
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines RN" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(5, size, RNPair.sep)
        } run { in =>
          assert(in.lines(RNPair).forall{_.last != '\n'})
        }
      }
      measure method "lines RN" in {
        having attribute ("version", "toString split on terminator") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            newIn(5, size, RNPair.sep)
          } run { in =>
            IOUtils.toString(in,"UTF-8").split("\r\n")
          }
        }
      }
      measure method "lines Custom" in {
        withSize upTo MaxSize by Inc withSetup { size =>
          newInResource(5, size, "**")
        } run { in =>
          in.lines(Custom("**")).force
        }
      }
      measure method "lines Custom" in {
        having attribute ("version", "java.io Reader") in {
          withSize upTo MaxSize by Inc withSetup { size =>
            newIn(5, size, "**")
          } run { in =>
            IOUtils.toString(in, "UTF-8").split("\\*\\*")
          }
        }
      }
    }
  }

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}