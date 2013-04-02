package scalax.io.perf
import Utils._
import sperformance.dsl.PerformanceDSLTest
import sperformance.Keys
import scalax.io.Input
import java.nio.channels.ReadableByteChannel
import scalax.io._
import Resource._
import StandardOpenOption._
import Line.Terminators._
import scalax.file._
import support.FileUtils
import java.nio.ByteBuffer
import java.nio.channels.FileChannel

trait AbstractFileSeekableTest extends AbstractSeekableTest {

  def MaxSize: Int
  def Inc: Int
  def From: Int

  override type Source = Path
  override def setup(size: Int,
    lines: Int = 2,
    term: String = NewLine.sep): Source = {
    val path = FileSystem.default.createTempFile().asInstanceOf[Path]
    path.write(generateTestData(size, lines, term))
    path
  }

  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   *
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  def newIn(source: Source,
    openOptions: Seq[OpenOption] = ReadWrite): () => SeekableByteChannel = () => {
    FileUtils.openChannel(source.toFile.get, openOptions)
  }

  def nioInsert(data: Array[Byte], pos: Int, size: Int) = {
    val source = setup(size)
    val chanFunc = newIn(source)
    val chan = chanFunc()
    chan.position(pos)
    val buffer = ByteBuffer.allocateDirect(chan.size - pos toInt)
    chan.read(buffer)
    buffer.flip()
    chan.position(pos)
    chan.write(ByteBuffer.wrap(data))
    chan.write(buffer)
    chan.close()
  }
  def nioPatch(data: Array[Byte], pos: Int, size: Int) = {
    val source = setup(size)
    val chanFunc = newIn(source)
    val chan = chanFunc()
    chan.position(pos)
    val buffer = ByteBuffer.wrap(data)
    chan.write(buffer)
    chan.close
  }
  def nioAppend(data: Array[Byte], size: Int) = {
    val source = setup(size)
    val chanFunc = newIn(source, openOptions = Seq(Create, Append))
    val chan = chanFunc()
    val buffer = ByteBuffer.wrap(data)
    chan.write(buffer)
    chan.close
  }

  performance of "Seekable" in {
    having attribute (Keys.WarmupRuns -> 10) in {
      measure method "patch bytes array" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
              (size, data.length, data)
            } run {
              case (size, pos, data) =>
                nioPatch(data, pos, size)
            }
          }
        }
      }
      measure method "patch bytes list" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
              (size, data.length, data.toList)
            } run {
              case (size, pos, data) =>
                nioPatch(data.toArray, pos, size)
            }
          }
        }
      }
      measure method "append strings" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              (size, generateTestData(size, 1))
            } run {
              case (size, data) =>
                nioAppend(data.getBytes(Codec.UTF8.name), size)
            }
          }
        }
      }
      measure method "append bytes array" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
              (size, data)
            } run {
              case (size, data) =>
                nioAppend(data, size)
            }
          }
        }
      }
      measure method "append bytes list" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              val data = generateTestData(size, 1).getBytes(Codec.UTF8.name).toList
              (size, data)
            } run {
              case (size, data) =>
                nioAppend(data.toArray, size)
            }
          }
        }
      }
      measure method "insert bytes array" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
              (size, data.length / 2, data)
            } run {
              case (size, pos, data) =>
                nioInsert(data, pos, size)
            }
          }
        }
      }
      measure method "insert bytes list" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              val data = generateTestData(size, 1).getBytes(Codec.UTF8.name).toList
              (size, data.length / 2, data)
            } run {
              case (size, pos, data) =>
                nioInsert(data.toArray, pos, size)
            }
          }
        }
      }
      measure method "bytes drop" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              (size)
            } run {
              case (size) =>
                val source = setup(size)
                val seekable = newIn(source)()

                val buffer = ByteBuffer.allocateDirect(size)
                seekable.position(size / 2)

                var read = seekable.read(buffer)
                while (read > 0) {
                  var i = 0
                  while (i < read) {
                    buffer.get(i)
                    i += 1
                  }
                  buffer.clear()
                  read = seekable.read(buffer)
                }
                seekable.close
            }
          }
        }
      }
      measure method "bytes take" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              (size)
            } run {
              case (size) =>
                val source = setup(size)
                val seekable = newIn(source)()
                val buffer = ByteBuffer.allocateDirect(size / 2)
                seekable.read(buffer)
                var i = 0
                while (buffer.hasRemaining()) {
                  buffer.get(i)
                  i += 1
                }
                seekable.close
            }
          }
        }
      }
      measure method "bytes zip" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              val data = generateTestData(size).getBytes(Codec.UTF8.name)
              (size, data.toList)
            } run {
              case (size, data) =>
                val source = setup(size)
                val seekable = newIn(source)()

                val buffer = ByteBuffer.allocateDirect(size)
                seekable.position(size / 2)

                var read = seekable.read(buffer)
                while (read > 0) {
                  var i = 0
                  while (i < read) {
                    (buffer.get(i), data(i))
                    i += 1
                  }
                  buffer.clear()
                  read = seekable.read(buffer)
                }
                seekable.close
            }
          }
        }
      }
      measure method "bytes limitFold" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              val data = generateTestData(size).getBytes(Codec.UTF8.name)
              (size, data.toList)
            } run {
              case (size, data) =>
                val source = setup(size)
                val seekable = newIn(source)()
                val buffer = ByteBuffer.allocateDirect(size / 2)
                seekable.read(buffer)
                var i = 0
                while (i < size / 2) {
                  buffer.get(i)
                  i += 1
                }
                seekable.close
            }
          }
        }
      }
      measure method "bytes apply" in {
        having attribute ("baseline", "") in {
          having attribute ("version", "std nio") in {
            withSizeDef { size =>
              size
            } run { size =>
              val source = setup(size)
              val seekable = newIn(source)()
              val buffer = ByteBuffer.allocateDirect(1)
              seekable.position(size / 2)
              seekable.read(buffer)
              buffer.get(0)
              seekable.close
            }
          }
        }
      }
    }
  }
}
