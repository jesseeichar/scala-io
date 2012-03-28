package scalax.io.perf

import Utils._
import scalax.io._
import sperformance.Keys
import sperformance.dsl._
import util.Random._
import Resource._
import Line.Terminators._
import java.io.ByteArrayInputStream
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.io.InputStream
import java.nio.channels.ReadableByteChannel
import java.nio.ByteBuffer
import java.nio.channels.Channels
import scalax.file.Path
import scalax.io.nio.SeekableFileChannel
import java.nio.channels.FileChannel

abstract class AbstractReadableByteChannelInputTest extends PerformanceDSLTest {
  implicit val codec = Codec.UTF8

  def MaxSize: Int
  def Inc: Int
  def From: Int

  def allowRandomAccess = false

  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   *
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep): () => ReadableByteChannel
  def newInResource(size: Int, lines: Int = 2, term: String = NewLine.sep): Input = {
    val in = newIn(size, lines, term)
    fromReadableByteChannel(in())
  }

  def withSizeDef[U](f: Int => U) = withSize from (From) upTo MaxSize by Inc withSetup (f)

  performance of "Input" in {
    having attribute (Keys.WarmupRuns -> 10) in {
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
              next <- api.next
            } i += 1
          }
        }
      }
      measure method "bytes" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop with buffer") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, inFunc) =>
                val in = inFunc()
                val buffer = ByteBuffer.allocateDirect(Buffers.BufferSize)
                var read = in.read(buffer)
                while (read > 0) {
                  buffer.flip()
                  var i = 0
                  while (i < read) {
                    buffer.get(i)
                    i += 1
                  }
                  buffer.clear()
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
      measure method "bytesAsInts" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop impl") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, inFunc) =>
                val in = inFunc()
                val buffer = ByteBuffer.allocateDirect(size * 2)
                var read = in.read(buffer)
                while (read > -1) {
                  var i = 0
                  while (i < read) {
                    buffer.get(i).toInt
                    i += 1
                  }
                  buffer.clear()
                  read = in.read(buffer)
                }
                in.close
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
      measure method "byteArray" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IO read bytes") in {
            withSizeDef { size =>
              newIn(size)
            } run { in =>
              val channel = in()
              IOUtils.toByteArray(Channels.newInputStream(channel));
              channel.close()
            }
          }
        }
      }
      measure method "copy to File" in {
        withSizeDef { size =>
          val in = newInResource(size)
          val out = Path.createTempFile(getClass.getSimpleName)
          (in, out)
        } run {
          case (in, out) =>
            in.copyDataTo(out)
        }
      }
      measure method "copy to File" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IO copy bytes") in {
            withSizeDef { size =>
              val in = newIn(size)
              val out = () => Path.createTempFile(getClass.getSimpleName).channel().open().get
              (in, out)
            } run {
              case (inFunc, outFunc) =>
                val in = inFunc()
                val out = outFunc()
                val inStream = Channels.newInputStream(in)
                IOUtils.copy(inStream, Channels.newOutputStream(out))
                inStream.close()
                in.close()
                out.close()
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
      measure method "copyDataTo" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IO copy bytes") in {
            withSizeDef { size =>
              val in = newIn(size)
              in
            } run {
              case inFunc =>
                val in = inFunc()
                val inStream = Channels.newInputStream(in)
                IOUtils.copy(inStream, NullOutputStream)
                inStream.close()
                in.close()
            }
          }
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
        having attribute ("version", "io.Source") in {
          withSizeDef { size =>
            newIn(size)
          } run { in =>
            val f = new CountFunction[Char]
            io.Source.fromInputStream(Channels.newInputStream(in()), "UTF-8").foreach(f)
          }
        }
      }
      measure method "chars" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io read chars with InputStreamReader") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, in) =>
                val buffer = new Array[Char](2048)
                val reader = Channels.newReader(in(), "UTF-8")
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
      measure method "lines newline" in {
        withSizeDef { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          val f = new CountFunction[String]
          in.lines(Line.Terminators.NewLine).foreach(f)
        }
      }
      measure method "lines newline" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IOUtils line") in {
            withSizeDef { size =>
              newIn(5, size, NewLine.sep)
            } run { inFunc =>
              val in = inFunc()
              val iter = IOUtils.lineIterator(Channels.newInputStream(in), "UTF-8")
              var i = 0
              while (iter.hasNext()) {
                iter.next()
                i += 1
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
            () => scala.io.Source.fromInputStream(Channels.newInputStream(in()), "UTF-8")
          } run { source =>
            val f = new CountFunction[String]
            source().getLines().foreach(f)
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
            val in = newIn(5, size, NewLine.sep)
            () => scala.io.Source.fromInputStream(Channels.newInputStream(in()), "UTF-8")
          } run { source =>
            val f = new CountFunction[String]
            source().getLines().foreach(f)
          }
        }
      }
      measure method "lines Auto" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "Apache IOUtils line") in {
            withSizeDef { size =>
              newIn(5, size, NewLine.sep)
            } run { inFunc =>
              val in = inFunc()
              val iter = IOUtils.lineIterator(Channels.newInputStream(in), "UTF-8")
              var i = 0
              while (iter.hasNext()) {
                iter.next()
                i += 1
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
      measure method "lines CR" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, NewLine.sep)
            () => scala.io.Source.fromInputStream(Channels.newInputStream(in()), "UTF-8")
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
              val iter = IOUtils.lineIterator(Channels.newInputStream(in), "UTF-8")
              var i = 0
              while (iter.hasNext()) {
                iter.next()
                i += 1
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
          in.lines(RNPair).foreach(f)
        }
      }
      measure method "lines RN" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, NewLine.sep)
            () => scala.io.Source.fromInputStream(Channels.newInputStream(in()), "UTF-8")
          } run { source =>
            val f = new CountFunction[String]
            source().getLines().foreach(f)
          }
        }
      }
      measure method "lines RN" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "toString split on terminator") in {
            withSizeDef { size =>

              newIn(5, size, RNPair.sep)
            } run { inFunc =>
              val in = inFunc()
              IOUtils.toString(Channels.newInputStream(in), "UTF-8").split("\r\n")
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
      measure method "lines Single Custom" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "toString split") in {
            withSizeDef { size =>
              newIn(5, size, "%")
            } run { inFunc =>
              val in = inFunc()
              IOUtils.toString(Channels.newInputStream(in), "UTF-8").split("%%")
              in.close()
            }
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
      measure method "lines Multi Custom" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "toString split") in {
            withSizeDef { size =>
              newIn(5, size, "**")
            } run { inFunc =>
              val in = inFunc()
              IOUtils.toString(Channels.newInputStream(in), "UTF-8").split("\\*\\*")
              in.close()
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
      measure method "bytes drop" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop with buffer") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, inFunc) =>
                val in = inFunc()

                val buffer = ByteBuffer.allocateDirect(size)
                buffer.clear.limit(size / 2)
                in.read(buffer)
                buffer.clear
                var read = in.read(buffer)
                while (read > 0) {
                  var i = 0
                  while (i < read) {
                    buffer.get(i)
                    i += 1
                  }
                  buffer.clear()
                  read = in.read(buffer)
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
      measure method "bytes take" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop with buffer") in {
            withSizeDef { size =>
              (size, newIn(size))
            } run {
              case (size, inFunc) =>
                val in = inFunc()

                val buffer = ByteBuffer.allocateDirect(size / 2)
                in.read(buffer)
                var i = 0
                while (buffer.hasRemaining()) {
                  buffer.get(i)
                  i += 1
                }
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
      measure method "bytes apply" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io while loop with buffer") in {
            withSizeDef { size =>
              (size / 4, newIn(size))
            } run {
              case (pos, inFunc) =>
                val in = inFunc()
                var current = 0L
                in match {
                  case sfc: SeekableByteChannel if allowRandomAccess =>
                    sfc.position(pos)
                    val buffer = Buffers.nioByteBuffer(Some(1))
                    in read buffer
                    buffer.get(0)
                  case fc: FileChannel if allowRandomAccess =>
                    fc.position(pos)
                    val buffer = Buffers.nioByteBuffer(Some(1))
                    in read buffer
                    buffer.get(0)
                  case _ =>
                    val buffer = Buffers.nioByteBuffer(Some(pos))
                    var i = 0L
                    while (pos - i > buffer.capacity()) {
                      buffer.clear()
                      i += in.read(buffer)
                    }

                    buffer.limit((pos - i).toInt)
                    in read buffer

                    buffer.get(buffer.limit - 1)
                }
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
      measure method "bytes head" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "java.io") in {
            withSizeDef { size =>
              newIn(size)
            } run {
              case inFunc =>
                val in = inFunc()
                val buffer = Buffers.nioByteBuffer(Some(1))
                in read buffer
                buffer.get(0)
                in.close
            }
          }
        }
      }
    }
  }
}