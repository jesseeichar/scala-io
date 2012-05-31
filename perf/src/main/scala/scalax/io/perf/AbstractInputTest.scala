package scalax.io.perf

import Utils._
import scalax.io._
import sperformance.Keys
import sperformance.dsl._
import util.Random._
import Resource._
import Line.Terminators._
import java.io.{ ByteArrayInputStream }
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.io.InputStream
import java.nio.channels.Channels

trait AbstractInputTest extends PerformanceDSLTest {
  implicit val codec = Codec.UTF8

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
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep): () => InputStream
  def newInResource(size: Int, lines: Int = 2, term: String = NewLine.sep): Input = {
    val in = newIn(size, lines, term)
    fromInputStream(in())
  }

  def withSizeDef[U](f: Int => U) = withSize from (From) upTo MaxSize by Inc withSetup (f)

  performance of "Input" in {
    having attribute (Keys.WarmupRuns -> 10) in {
      measure method "bytes" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop with buffer") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, inFunc) =>
                val in = inFunc()
                val buffer = new Array[Byte](Buffers.BufferSize)
                var read = in.read(buffer)
                while (read > 0) {
                  var i = 0
                  while (i < read) {
                    buffer(i)
                    i += 1
                  }
                  read = in.read(buffer)
                }
                in.close
            }
          }
        }
      }
      measure method "bytes" in {
        withSizeDef { size =>
          newInResource(size)
        } run { in =>
          val f = new CountFunction[Byte]
          in.bytes.foreach(f)
        }
      }
      measure method "bytes" in {
        having attribute ("version", "scala.io block read") in {
          withSizeDef { size =>
            newInResource(size)
          } run { in =>
            val f = new Function1[ByteBlock, Unit] {
              def apply(b: ByteBlock) = {
                var i = b.size
                while (i > 0) {
                  i -= 1
                  b(i)
                }
              }
            }
            in.blocks().foreach(f)
          }
        }
      }
      measure method "bytes" in {
        having attribute ("version", "scala.io processor read") in {
          withSizeDef { size =>
            newInResource(size)
          } run { in =>
            var i = 0
            for {
              api <- in.bytes.processor
              _ <- api.repeatUntilEmpty()
              next <- api.next
            } i += 1
          }
        }
      }
      measure method "bytesAsInts" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop impl") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, inFunc) =>
                val in = inFunc()
                val buffer = new Array[Byte](Buffers.BufferSize)
                var read = in.read(buffer)
                while (read > -1) {
                  var i = 0
                  while (i < read) {
                    buffer(i)
                    i += 1
                  }

                  read = in.read(buffer)
                }
                in.close
            }
          }
        }
      }
      measure method "bytesAsInts" in {
        withSizeDef { size =>
          newInResource(size)
        } run { in =>
          val f = new CountFunction[Int]
          in.bytesAsInts.foreach(f)
        }
      }
      measure method "byteArray" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IO read bytes") in {
            withSizeDef { size =>
              newIn(size)
            } run { in =>
              IOUtils.toByteArray(in())
            }
          }
        }
      }
      measure method "byteArray" in {
        withSizeDef { size =>
          newInResource(size)
        } run { in =>
          in.byteArray
        }
      }
      measure method "copyDataTo" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IO copy bytes") in {
            withSizeDef { size =>
              val in = newIn(size)
              val out = NullOutputStream
              (in, out)
            } run {
              case (in, out) =>
                IOUtils.copy(in(), out)
            }
          }
        }
      }
      measure method "copyDataTo" in {
        withSizeDef { size =>
          val in = newInResource(size)
          val out = fromOutputStream(NullOutputStream)
          (in, out)
        } run {
          case (in, out) =>
            in.copyDataTo(out)
        }
      }
      measure method "chars" in {
        withSizeDef { size =>
          newInResource(size)
        } run { in =>
          val f = new CountFunction[Char]
          in.chars.foreach(f)
        }
      }
      measure method "chars" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io read chars with InputStreamReader") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, in) =>
                val buffer = new Array[Char](size * 2)
                val reader = new InputStreamReader(in(), Charset.forName("UTF-8"))
                var read = 0

                do {
                  read = reader.read(buffer)
                  var i = 0
                  while (i < read) {
                    buffer(i)
                    i += 1
                  }

                } while (read > 0)
                reader.close()
            }
          }
        }
      }
      measure method "chars" in {
        having attribute ("version", "io.Source") in {
          withSizeDef { size =>
            newIn(size)
          } run { in =>
            val f = new CountFunction[Char]
            io.Source.fromInputStream(in(), "UTF-8").foreach(f)
          }
        }
      }
      measure method "lines newline" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IOUtils line") in {
            withSizeDef { size =>
              newIn(5, size, NewLine.sep)
            } run { inFunc =>
              val in = inFunc()
              val iter = IOUtils.lineIterator(in, "UTF-8")
              var i = 0
              while (iter.hasNext()) {
                i += 1
                iter.next()
              }
              in.close()
            }
          }
        }
      }
      measure method "lines newline" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, NewLine.sep)
            () => scala.io.Source.fromInputStream(in(), "UTF-8")
          } run { source =>
            val f = new CountFunction[String]
            source().getLines().foreach(f)
          }
        }
      }
      measure method "lines newline" in {
        withSizeDef { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          val f = new CountFunction[String]
          in.lines(Line.Terminators.NewLine).foreach(f)
        }
      }
      measure method "lines Auto" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IOUtils line") in {
            withSizeDef { size =>
              newIn(5, size, NewLine.sep)
            } run { inFunc =>
              val in = inFunc()
              val iter = IOUtils.lineIterator(in, "UTF-8")
              while (iter.hasNext()) {
                iter.next()
              }
              in.close()
            }
          }
        }
      }
      measure method "lines Auto" in {
        withSizeDef { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          val f = new CountFunction[String]
          in.lines(Line.Terminators.Auto).foreach(f)
        }
      }
      measure method "lines Auto" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            import JavaConverters._
            val in = newIn(5, size, NewLine.sep)
            () => scala.io.Source.fromInputStream(in(), "UTF-8")
          } run { source =>
            val f = new CountFunction[String]
            source().getLines().foreach(f)
          }
        }
      }
      measure method "lines CR" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, CarriageReturn.sep)
            () => scala.io.Source.fromInputStream(in(), "UTF-8")
          } run { source =>
            val f = new CountFunction[String]
            source().getLines().foreach(f)
          }
        }
      }
      measure method "lines CR" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IOUtils line") in {
            withSizeDef { size =>
              newIn(5, size, CarriageReturn.sep)
            } run { inFunc =>
              val in = inFunc()
              val iter = IOUtils.lineIterator(in, "UTF-8")
              while (iter.hasNext()) {
                iter.next()
              }
              in.close()
            }
          }
        }
      }
      measure method "lines CR" in {
        withSizeDef { size =>
          newInResource(5, size, CarriageReturn.sep)
        } run { in =>
          val f = new CountFunction[String]
          in.lines(Line.Terminators.CarriageReturn).foreach(f)
        }
      }
      measure method "lines RN" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IOUtils line") in {
            withSizeDef { size =>
              newIn(5, size, NewLine.sep)
            } run { inFunc =>
              val in = inFunc()
              val iter = IOUtils.lineIterator(in, "UTF-8")
              while (iter.hasNext()) {
                iter.next()
              }
              in.close()
            }
          }
        }
      }
      measure method "lines RN" in {
        withSizeDef { size =>
          newInResource(5, size, RNPair.sep)
        } run { in =>
          val f = new CountFunction[String]
          in.lines(Line.Terminators.RNPair).foreach(f)
        }
      }
      measure method "lines RN" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, RNPair.sep)
            () => scala.io.Source.fromInputStream(in(), "UTF-8")
          } run { source =>
            val f = new CountFunction[String]
            source().getLines().foreach(f)
          }
        }
      }
      measure method "lines RN" in {
        having attribute ("version", "toString split on terminator") in {
          withSizeDef { size =>
            newIn(5, size, RNPair.sep)
          } run { inFunc =>
            val in = inFunc()
            IOUtils.toString(in, "UTF-8").split("\r\n")
            in.close()
          }
        }
      }
      measure method "lines Single Custom" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "tiString split") in {
            withSizeDef { size =>
              newIn(5, size, "%")
            } run { inFunc =>
              val in = inFunc()
              IOUtils.toString(in, "UTF-8").split("%")
              in.close()
            }
          }
        }
      }
      measure method "lines Single Custom" in {
        withSizeDef { size =>
          newInResource(5, size, "%")
        } run { in =>
          val f = new CountFunction[String]
          in.lines(Custom("%")).foreach(f)
        }
      }
      measure method "lines Multi Custom" in {
        having attribute ("version", "toString split") in {
          withSizeDef { size =>
            newIn(5, size, "**")
          } run { inFunc =>
            val in = inFunc()
            IOUtils.toString(in, "UTF-8").split("\\*\\*")
            in.close()
          }
        }
      }
      measure method "lines Multi Custom" in {
        withSizeDef { size =>
          newInResource(5, size, "**")
        } run { in =>
          val f = new CountFunction[String]
          in.lines(Custom("**")).foreach(f)
        }
      }
      measure method "bytes drop" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop with buffer") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, inFunc) =>
                val in = inFunc()
                in.skip(size / 2)
                val buffer = new Array[Byte](size)
                var read = in.read(buffer)
                while (read > 0) {
                  var i = 0
                  while (i < read) {
                    buffer(i)
                    i += 1
                  }
                  read = in.read(buffer)
                }
                in.close
            }
          }
        }
      }
      measure method "bytes drop" in {
        withSizeDef { size =>
          (size / 2, newInResource(size))
        } run {
          case (toDrop, in) =>
            val f = new CountFunction[Byte]
            in.bytes.drop(toDrop).foreach(f)
        }
      }
      measure method "bytes take" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop with buffer") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, inFunc) =>
                val in = inFunc()

                val buffer = new Array[Byte](size)
                val read = in.read(buffer)
                var i = 0
                while (i < read) {
                  buffer(i)
                  i += 1
                }
                in.close
            }
          }
        }
      }
      measure method "bytes take" in {
        withSizeDef { size =>
          (size / 2, newInResource(size))
        } run {
          case (toTake, in) =>
            val f = new CountFunction[Byte]
            in.bytes.take(toTake).foreach(f)
        }
      }
      measure method "bytes apply" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop with buffer") in {
            withSizeDef { size =>
              (size / 4, newIn(size))
            } run {
              case (pos, inFunc) =>
                val in = inFunc()
                in.skip(pos)
                in read ()
                in.close
            }
          }
        }
      }
      measure method "bytes apply" in {
        withSizeDef { size =>
          (size / 4, newInResource(size))
        } run {
          case (pos, in) =>
            in.bytes.apply(pos)
        }
      }
      measure method "bytes head" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io") in {
            withSizeDef { size =>
              newIn(size)
            } run {
              case inFunc =>
                val in = inFunc()
                val buffer = Buffers.arrayBuffer(Some(1))
                in read buffer
                buffer(0)
                in.close
            }
          }
        }
      }
      measure method "bytes head" in {
        withSizeDef { size =>
          newInResource(size)
        } run {
          case in =>
            in.bytes.head
        }
      }
    }
  }
}
