package scalax.io.perf
import util.Random._
import sperformance.dsl.PerformanceDSLTest
import sperformance.Keys
import scalax.io.Input
import java.nio.channels.ReadableByteChannel
import scalax.io._
import Resource._
import StandardOpenOption._
import Line.Terminators._
import scalax.file._
import defaultfs.DefaultPath
import support.FileUtils
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import nio.SeekableFileChannel

trait AbstractFileSeekableTest extends AbstractSeekableTest {

  def MaxSize: Int
  def Inc: Int
  def From: Int
  def WarmUpRuns: Int

  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   *
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  override def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep, openOptions: Seq[OpenOption] = ReadWrite) = () => {
    val path = FileSystem.default.createTempFile()
    path.write(generateTestData(size, lines, term))
    FileUtils.openChannel(path.asInstanceOf[DefaultPath].jfile, openOptions)
  }

  def nioInsert(data: Array[Byte], pos: Int, chanFunc: () => SeekableFileChannel) = {
    val chan = chanFunc()
    chan.position(pos)
    val buffer = ByteBuffer.allocateDirect(data.length)
    chan.read(buffer)
    buffer.flip()
    chan.position(chan.size)
    chan.write(buffer)
    chan.position(pos)
    chan.write(ByteBuffer.wrap(data))
  }
  def nioPatch(data: Array[Byte], pos: Int, chanFunc: () => SeekableFileChannel) = {
    val chan = chanFunc()
    chan.position(pos)
    val buffer = ByteBuffer.wrap(data)
    chan.write(buffer)
  }
  def nioAppend(data: Array[Byte], chanFunc: () => SeekableFileChannel) = {
    val chan = chanFunc()
    val buffer = ByteBuffer.wrap(data)
    chan.write(buffer)
  }

  performance of "Seekable" in {
    having attribute (Keys.WarmupRuns -> WarmUpRuns) in {
      measure method "patch strings" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            (size / 2, generateTestData(size, 1), newIn(size))
          } run {
            case (pos, data, channelFun) =>
              val array = data.getBytes(Codec.ISO8859.name)
              nioPatch(array, pos, channelFun)
          }
        }
      }
      measure method "patch bytes array" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
            (data.length, data, newIn(size))
          } run {
            case (pos, data, channelFun) =>
              nioPatch(data, pos, channelFun)
          }
        }
      }
      measure method "patch bytes list" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
            (data.length, data.toList, newIn(size))
          } run {
            case (pos, data, channelFun) =>
              nioPatch(data.toArray, pos, channelFun)
          }
        }
      }
      measure method "append strings" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            (generateTestData(size, 1), newIn(size, openOptions = Seq(Create, Append)))
          } run {
            case (data, channelFun) =>
              nioAppend(data.getBytes(Codec.UTF8.name), channelFun)
          }
        }
      }
      measure method "append bytes array" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
            (data, newIn(size, openOptions = Seq(Create, Append)))
          } run {
            case (data, channelFun) =>
              nioAppend(data, channelFun)
          }
        }
      }
      measure method "append bytes list" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            val data = generateTestData(size, 1).getBytes(Codec.UTF8.name).toList
            (data, newIn(size, openOptions = Seq(Create, Append)))
          } run {
            case (data, channelFun) =>
              nioAppend(data.toArray, channelFun)
          }
        }
      }
      measure method "insert strings" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            val data = generateTestData(size, 1)
            (data.length, data, newIn(size))
          } run {
            case (pos, data, channelFun) =>
              nioInsert(data.getBytes(Codec.UTF8.name), pos, channelFun)
          }
        }
      }
      measure method "insert bytes array" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
            (data.length / 2, data, newIn(size))
          } run {
            case (pos, data, channelFun) =>
              nioInsert(data, pos, channelFun)
          }
        }
      }
      measure method "insert bytes list" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            val data = generateTestData(size, 1).getBytes(Codec.UTF8.name).toList
            (data.length / 2, data, newIn(size))
          } run {
            case (pos, data, channelFun) =>
              nioInsert(data.toArray, pos, channelFun)
          }
        }
      }
    }
  }
}